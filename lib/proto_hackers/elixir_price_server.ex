defmodule ProtoHackers.ElixirPriceServer do
  use GenServer

  require Logger

  alias ProtoHackers.ElixirPriceServer.Session

  @port 4205

  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl true
  def init([]) do
    {:ok, socket} =
      :gen_tcp.listen(@port,
        mode: :binary,
        active: false,
        reuseaddr: true,
        exit_on_close: false,
        buffer: 1024,
        backlog: 10_000
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
        Session.Supervisor.start_child(client_socket)

      {:error, :timeout} ->
        :ok

      {:error, reason} ->
        Logger.error("Error in `accept`: #{inspect(reason)}")
    end

    {:noreply, state}
  end
end
