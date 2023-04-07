defmodule ProtoHackers.ElixirChatServer.Session do
  use GenServer

  require Logger

  alias ProtoHackers.ElixirChatServer.Presence

  defmodule State do
    @enforce_keys [:socket, :stage, :username, :ref]
    defstruct @enforce_keys
  end

  def start_link(socket) do
    GenServer.start_link(__MODULE__, socket)
  end

  @impl true
  def init(socket) do
    :gen_tcp.send(socket, "Welcome! What is your username?\n")

    ref = make_ref()

    send(self(), :read)

    {:ok, %State{socket: socket, stage: :read_username, username: nil, ref: ref}}
  end

  @impl true
  def handle_info(:read, %State{socket: socket, stage: :read_username, ref: ref} = state) do
    send(self(), :read)

    case :gen_tcp.recv(socket, 0) do
      {:ok, data} ->
        username = String.trim(data)

        if valid_username?(username) do
          users_string = Enum.join(Presence.get_users(), ", ")
          Presence.Bus.user_joined(username, ref)
          Presence.Bus.subscribe()
          :gen_tcp.send(socket, "* Users: #{users_string}\n")

          {:noreply, %State{state | stage: :read_message, username: username}}
        else
          :gen_tcp.close(socket)

          {:stop, :normal, state}
        end

      {:error, :timeout} ->
        {:noreply, state}

      {:error, reason} ->
        Presence.Bus.user_left(state.username, state.ref)

        {:stop, reason, state}
    end
  end

  @impl true
  def handle_info(:read, %State{socket: socket, stage: :read_message} = state) do
    send(self(), :read)

    case :gen_tcp.recv(socket, 0, 25) do
      {:ok, data} ->
        message = String.trim(data)

        if message != "" do
          Presence.Bus.user_sent_message(state.username, state.ref, message)
        end

        {:noreply, state}

      {:error, :timeout} ->
        {:noreply, state}

      {:error, :closed} ->
        Presence.Bus.user_left(state.username, state.ref)

        {:stop, :normal, state}

      {:error, reason} ->
        {:stop, reason, state}
    end
  end

  def handle_info({:user_joined, username, ref}, %State{ref: our_ref} = state)
      when ref != our_ref do
    :gen_tcp.send(state.socket, "* #{username} has joined\n")

    {:noreply, state}
  end

  def handle_info({:user_left, username, ref}, %State{ref: our_ref} = state)
      when ref != our_ref do
    :gen_tcp.send(state.socket, "* #{username} has left\n")

    {:noreply, state}
  end

  def handle_info({:user_sent_message, _username, ref, _message}, %State{ref: ref} = state) do
    {:noreply, state}
  end

  def handle_info({:user_sent_message, username, _ref, message}, %State{} = state) do
    :gen_tcp.send(state.socket, "[#{username}] #{message}\n")

    {:noreply, state}
  end

  defp valid_username?(username) do
    String.length(username) > 0 && username |> String.to_charlist() |> Enum.all?(&alphanumeric?/1)
  end

  defp alphanumeric?(char) do
    char in ?a..?z || char in ?A..?Z || char in ?0..?9
  end
end
