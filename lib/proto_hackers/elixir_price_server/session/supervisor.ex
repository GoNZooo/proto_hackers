defmodule ProtoHackers.ElixirPriceServer.Session.Supervisor do
  # Automatically defines child_spec/1
  use DynamicSupervisor

  alias ProtoHackers.ElixirPriceServer.Session

  def start_link([]) do
    DynamicSupervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def start_child(socket) do
    spec = %{
      id: {Session, socket},
      start: {Session, :start_link, [socket]},
      restart: :temporary,
      shutdown: :brutal_kill,
      type: :worker
    }

    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init([]) do
    DynamicSupervisor.init(strategy: :one_for_one, extra_arguments: [], max_children: 10_000)
  end
end
