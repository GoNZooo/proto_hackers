defmodule ProtoHackers.ElixirChatServer do
  use GenServer

  require Logger

  alias ProtoHackers.ElixirChatServer.Session

  @port 4207

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    {:ok, socket} =
      :gen_tcp.listen(@port,
        mode: :binary,
        active: false,
        exit_on_close: false,
        reuseaddr: true,
        backlog: 500,
        packet: :line
      )

    send(self(), :accept)

    {:ok, %{socket: socket}}
  end

  @impl true
  def handle_info(:accept, %{socket: socket} = state) do
    send(self(), :accept)

    case :gen_tcp.accept(socket) do
      {:ok, client_socket} ->
        Session.Supervisor.start_child(client_socket)

        {:noreply, state}

      {:error, reason} ->
        Logger.error("Error accepting connection: #{inspect(reason)}")

        {:stop, reason, state}
    end
  end
end
