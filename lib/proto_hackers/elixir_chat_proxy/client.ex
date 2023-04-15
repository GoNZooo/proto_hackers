defmodule ProtoHackers.ElixirChatProxy.Client do
  use GenServer

  require Logger

  alias ProtoHackers.ElixirChatProxy.Boguscoin

  @timeout 50
  @bobs_address "7YWHMfk9JZe0LM0g1ZauHuiSxhI"

  def start_link(socket) do
    GenServer.start_link(__MODULE__, socket)
  end

  @impl true
  def init(socket) do
    case :gen_tcp.connect(
           'chat.protohackers.com',
           16963,
           [active: false, mode: :binary, packet: :line],
           250
         ) do
      {:ok, upstream} ->
        send(self(), :read)

        {:ok, %{socket: socket, upstream: upstream}}

      {:error, reason} ->
        Logger.error("#{inspect(socket)} failed to connect to upstream: #{inspect(reason)}")

        {:stop, :failed_to_connect_to_upstream, %{socket: socket}}
    end
  end

  @impl true
  def handle_info(:read, %{socket: socket, upstream: upstream} = state) do
    send(self(), :read)

    case :gen_tcp.recv(socket, 0, @timeout) do
      {:ok, data} ->
        Logger.debug("#{inspect(socket)} sent '#{inspect(data)}' to upstream")
        rewritten_data = Boguscoin.rewrite(data, @bobs_address)
        :gen_tcp.send(upstream, rewritten_data)

      {:error, :closed} ->
        # Logger.debug("#{inspect(socket)} closed")
        :gen_tcp.close(upstream)

      {:error, :timeout} ->
        :ok
    end

    case :gen_tcp.recv(upstream, 0, @timeout) do
      {:ok, data} ->
        Logger.debug("Upstream sent '#{inspect(data)}' to #{inspect(socket)}")
        rewritten_data = Boguscoin.rewrite(data, @bobs_address)
        :gen_tcp.send(socket, rewritten_data)

      {:error, :closed} ->
        :gen_tcp.close(socket)

      {:error, :timeout} ->
        :ok
    end

    {:noreply, state}
  end
end
