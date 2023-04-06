defmodule ProtoHackers.ElixirPriceServer.SessionThroughPurs do
  alias ProtoHackers.ElixirPriceServer.SessionThroughPurs.Handling

  def start_link(socket) do
    :simpleGenServer@ps.startLink(%{socket: socket}, %{
      name: {:nothing},
      init: &Handling.init/1,
      handleInfo: &Handling.handle_info/1
    }).()
  end
end
