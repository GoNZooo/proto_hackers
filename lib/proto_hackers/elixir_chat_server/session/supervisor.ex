defmodule ProtoHackers.ElixirChatServer.Session.Supervisor do
  use DynamicSupervisor

  def start_link() do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_child(socket) do
    spec = %{
      id: :proto_hackers,
      start: {ProtoHackers.ElixirChatServer.Session, :start_link, [socket]},
      restart: :temporary,
      shutdown: 5000,
      type: :worker
    }

    DynamicSupervisor.start_child(__MODULE__, spec)
  end
end
