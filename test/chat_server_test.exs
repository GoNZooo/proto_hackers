defmodule ChatServerTest do
  use ExUnit.Case

  require Logger

  @ports [4207]

  test "user is asked for their username when connecting" do
    @ports
    |> Enum.map(fn port ->
      Task.async(fn ->
        {:ok, socket} = :gen_tcp.connect({127, 0, 0, 1}, 4207, mode: :binary, active: false)

        {:ok, data} = :gen_tcp.recv(socket, 0)
        assert data == "Welcome! What is your username?"
      end)
    end)
  end
end
