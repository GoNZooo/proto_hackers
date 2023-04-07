defmodule ProtoHackers.ElixirChatServer.Presence do
  use GenServer

  defmodule State do
    @enforce_keys [:users]
    defstruct @enforce_keys
  end

  defmodule Bus do
    def subscribe() do
      :pg.join(__MODULE__, self())
    end

    def unsubscribe() do
      :pg.leave(__MODULE__, self())
    end

    def user_joined(username, ref) do
      broadcast({:user_joined, username, ref})
    end

    def user_left(username, ref) do
      broadcast({:user_left, username, ref})
    end

    def user_sent_message(username, ref, message) do
      broadcast({:user_sent_message, username, ref, message})
    end

    def broadcast(message) do
      __MODULE__
      |> :pg.get_members()
      |> Enum.each(fn pid -> send(pid, message) end)
    end
  end

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def get_users() do
    GenServer.call(__MODULE__, :get_users)
  end

  @impl true
  def init(:ok) do
    Bus.subscribe()

    {:ok, %State{users: Map.new()}}
  end

  @impl true
  def handle_call(:get_users, _from, state) do
    {:reply, state.users |> Map.values() |> Enum.sort(), state}
  end

  @impl true
  def handle_info({:user_joined, username, ref}, state) do
    {:noreply, %{state | users: Map.put(state.users, ref, username)}}
  end

  @impl true
  def handle_info({:user_left, _username, ref}, state) do
    {:noreply, %{state | users: Map.delete(state.users, ref)}}
  end

  @impl true
  def handle_info({:user_sent_message, _username, _ref, _message}, state) do
    {:noreply, state}
  end
end
