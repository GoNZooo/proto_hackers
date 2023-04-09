defmodule ProtoHackers.ElixirKeyValueStore do
  use GenServer

  require Logger

  @version 0.1
  @version_string "ProtoHackers.ElixirKeyValueStore-#{@version}"

  defmodule State do
    @enforce_keys [:socket, :store]
    defstruct [:socket, :store]
  end

  defmodule Commands do
    @max_command_size 1000

    defmodule Version do
      @enforce_keys []
      defstruct @enforce_keys
    end

    defmodule Insert do
      @enforce_keys [:key, :value]
      defstruct @enforce_keys
    end

    defmodule Query do
      @enforce_keys [:key]
      defstruct @enforce_keys
    end

    def parse("version") do
      {:ok, %Version{}}
    end

    def parse(value) when is_binary(value) and byte_size(value) < @max_command_size do
      case String.split(value, "=", parts: 2) do
        ["version", value] ->
          {:error, {:invalid_command, value}}

        [key, value] ->
          {:ok, %Insert{key: key, value: value}}

        [value] ->
          {:ok, %Query{key: value}}

        _other ->
          {:error, {:invalid_command, value}}
      end
    end

    def parse(value) do
      {:error, {:invalid_size, value}}
    end
  end

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    {:ok, socket} = :gen_udp.open(4201, mode: :binary, active: false, reuseaddr: true)
    send(self(), :read)

    {:ok, %State{socket: socket, store: %{"version" => @version_string}}}
  end

  @impl true
  def handle_info(:read, state) do
    send(self(), :read)

    case :gen_udp.recv(state.socket, 0, 50) do
      {:ok, {address, port, data}} ->
        case Commands.parse(data) do
          {:ok, %Commands.Version{}} ->
            :gen_udp.send(state.socket, address, port, @version_string)

            {:noreply, state}

          {:ok, %Commands.Insert{key: key, value: value}} ->
            {:noreply, %State{state | store: Map.put(state.store, key, value)}}

          {:ok, %Commands.Query{key: key}} ->
            case Map.fetch(state.store, key) do
              {:ok, value} ->
                :gen_udp.send(state.socket, address, port, "#{key}=#{value}")
                {:noreply, state}

              :error ->
                {:noreply, state}
            end

          {:error, {:invalid_size, _value}} ->
            {:noreply, state}

          {:error, {:invalid_command, _value}} ->
            {:noreply, state}
        end

      {:error, :timeout} ->
        {:noreply, state}

      {:error, reason} ->
        Logger.error("Error receiving UDP packet: #{inspect(reason)}")

        {:noreply, state}
    end
  end
end
