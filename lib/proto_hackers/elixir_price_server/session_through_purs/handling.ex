defmodule ProtoHackers.ElixirPriceServer.SessionThroughPurs.Handling do
  require Logger

  defmodule State do
    @enforce_keys [:socket, :prices]
    defstruct @enforce_keys
  end

  defmodule Request do
    defmodule Insert do
      @enforce_keys [:timestamp, :price]
      defstruct @enforce_keys
    end

    defmodule Query do
      @enforce_keys [:minimum_time, :maximum_time]
      defstruct @enforce_keys
    end

    def parse(
          <<"I"::binary, timestamp::big-signed-integer-size(32),
            price::big-signed-integer-size(32)>>
        ) do
      {:ok, %Insert{timestamp: timestamp, price: price}}
    end

    def parse(
          <<"Q"::binary, minimum_time::big-signed-integer-size(32),
            maximum_time::big-signed-integer-size(32)>>
        ) do
      {:ok, %Query{minimum_time: minimum_time, maximum_time: maximum_time}}
    end

    def parse(_other) do
      {:error, :invalid_request}
    end

    def encode(%Insert{timestamp: timestamp, price: price}) do
      <<"I", timestamp::big-signed-integer-size(32), price::big-integer-size(32)>>
    end

    def encode(%Query{minimum_time: minimum_time, maximum_time: maximum_time}) do
      <<"Q", minimum_time::big-signed-integer-size(32), maximum_time::big-integer-size(32)>>
    end
  end

  def init(%{socket: socket}) do
    fn ->
      send(self(), :await_request)

      {:simpleInitOk, %State{socket: socket, prices: MapSet.new()}}
    end
  end

  def handle_info(message), do: fn state -> handle_info(message, state) end

  def handle_info(:await_request, %State{socket: socket, prices: prices} = state) do
    fn ->
      new_state =
        case get_request(socket) do
          {:ok, %Request.Insert{} = insert} ->
            new_prices = handle_insert(insert, prices)
            send(self(), :await_request)
            %State{state | prices: new_prices}

          {:ok, %Request.Query{} = query} ->
            handle_query(socket, query, prices)
            send(self(), :await_request)
            state

          {:error, :invalid_request} ->
            send(self(), :await_request)
            state

          {:error, :closed} ->
            send(self(), :close)
            state

          {:error, :timeout} ->
            send(self(), :await_request)
            state

          {:error, reason} ->
            send(self(), :await_request)
            Logger.error("Error when awaiting request: #{inspect(reason)}")

            state
        end

      {:simpleNoReply, new_state}
    end
  end

  def handle_info(:close, %State{socket: socket} = state) do
    fn ->
      :gen_tcp.close(socket)

      {:simpleStop, {:stopNormal}, state}
    end
  end

  defp get_request(socket) do
    case :gen_tcp.recv(socket, 9) do
      {:ok, data} ->
        Request.parse(data)

      {:error, :closed} ->
        {:error, :closed}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_insert(%Request.Insert{timestamp: timestamp, price: price}, prices) do
    MapSet.put(prices, %{timestamp: timestamp, price: price})
  end

  defp handle_query(
         socket,
         %Request.Query{minimum_time: minimum_time, maximum_time: maximum_time},
         prices
       ) do
    mean_price =
      prices
      |> MapSet.to_list()
      |> Enum.filter(fn %{timestamp: timestamp} ->
        timestamp >= minimum_time and timestamp <= maximum_time
      end)
      |> case do
        [] ->
          0

        prices_in_range ->
          prices_in_range
          |> Enum.map(fn %{price: price} -> price end)
          |> Enum.sum()
          |> Kernel./(length(prices_in_range))
          |> round()
      end

    :gen_tcp.send(socket, <<mean_price::big-integer-size(32)>>)
  end
end
