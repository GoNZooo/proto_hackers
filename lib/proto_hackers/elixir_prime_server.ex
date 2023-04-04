defmodule ProtoHackers.ElixirPrimeServer do
  use GenServer

  require Logger

  alias ProtoHackers.Prime

  @packet_size 1024
  @port 4203

  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl true
  def init([]) do
    {:ok, socket} =
      :gen_tcp.listen(@port,
        mode: :binary,
        active: false,
        packet_size: @packet_size,
        reuseaddr: true,
        exit_on_close: false
      )

    Logger.debug("#{__MODULE__} listening on port #{@port}: #{inspect(socket, pretty: true)}")

    send(self(), :accept_clients)

    {:ok, %{socket: socket}}
  end

  @impl true
  def handle_info(:accept_clients, %{socket: socket} = state) do
    send(self(), :accept_clients)

    case :gen_tcp.accept(socket) do
      {:ok, client_socket} ->
        spawn(fn -> handle_client(client_socket) end)

      {:error, :timeout} ->
        :ok

      {:error, reason} ->
        Logger.error("Error: #{inspect(reason)}")
    end

    {:noreply, state}
  end

  defp handle_client(socket) do
    case read_line(socket) do
      {:ok, %{"method" => "isPrime", "number" => number}} when is_integer(number) ->
        response = %{"method" => "isPrime", "prime" => Prime.prime?(number)}
        :gen_tcp.send(socket, [Jason.encode!(response), "\n"])

        handle_client(socket)

      {:ok, %{"method" => "isPrime", "number" => number}} when is_float(number) ->
        response = %{"method" => "isPrime", "prime" => false}
        :gen_tcp.send(socket, [Jason.encode!(response), "\n"])

        handle_client(socket)

      {:error, :closed} ->
        :ok

      _other ->
        :gen_tcp.send(socket, "malformed request")
    end

    :gen_tcp.close(socket)
  end

  defp read_line(socket) do
    case :gen_tcp.recv(socket, 0) do
      {:ok, data} ->
        Jason.decode(data)

      {:error, :timeout} ->
        read_line(socket)

      {:error, :closed} ->
        {:error, :closed}

      {:error, reason} ->
        Logger.error("Error in `read_line`: #{inspect(reason)}")
    end
  end
end
