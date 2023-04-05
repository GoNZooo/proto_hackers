defmodule ProtoHackers.ElixirPriceServer.Session.Supervisor do
  # Automatically defines child_spec/1
  use DynamicSupervisor

  def start_link([]) do
    DynamicSupervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def start_child(socket) do
    spec = {ProtoHackers.ElixirPriceServer.Session, socket}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init([]) do
    DynamicSupervisor.init(
      strategy: :one_for_one,
      extra_arguments: []
    )
  end
end
