defmodule ProtoHackers.ElixirChatProxy do
  use GenServer

  require Logger

  alias ProtoHackers.ElixirChatProxy.Client.Supervisor, as: ClientSupervisor

  @port 4209

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    {:ok, socket} =
      :gen_tcp.listen(@port, mode: :binary, active: false, reuseaddr: true, packet: :line)

    Logger.info("'#{__MODULE__}' Listening on port #{@port}")

    send(self(), :accept)

    {:ok, %{socket: socket, connections: Map.new()}}
  end

  @impl true
  def handle_info(:accept, %{socket: socket} = state) do
    send(self(), :accept)

    case :gen_tcp.accept(socket) do
      {:ok, client_socket} ->
        ClientSupervisor.start_child(client_socket)

        {:noreply, state}

      {:error, _error} ->
        {:noreply, state}
    end
  end
end
