defmodule ProtoHackers.ElixirTcpEchoServer do
  use GenServer

  require Logger

  @packet_size 1024

  def start_link([]) do
    GenServer.start_link(__MODULE__, :no_init_value, name: __MODULE__)
  end

  @impl true
  def init(:no_init_value) do
    {:ok, socket} =
      :gen_tcp.listen(4201,
        mode: :binary,
        active: false,
        packet_size: @packet_size,
        reuseaddr: true,
        # backlog: 5,
        exit_on_close: false
      )

    Logger.debug("Listening on port 4201: #{inspect(socket, pretty: true)}")

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
    case receive_all_data(socket) do
      {:ok, data} ->
        :gen_tcp.send(socket, data)

      {:error, reason} ->
        Logger.error("Error: #{inspect(reason)}")
    end

    :gen_tcp.close(socket)
  end

  defp receive_all_data(socket) do
    receive_all_data(socket, "")
  end

  defp receive_all_data(socket, io_list) do
    case :gen_tcp.recv(socket, 0, 10_000) do
      {:ok, data} ->
        receive_all_data(socket, [io_list, data])

      {:error, :closed} ->
        {:ok, io_list}

      {:error, reason} ->
        :gen_tcp.close(socket)

        {:error, reason}
    end
  end
end
