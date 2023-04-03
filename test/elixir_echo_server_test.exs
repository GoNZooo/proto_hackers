defmodule ElixirEchoServerTest do
  use ExUnit.Case

  @ip {127, 0, 0, 1}
  @port 4201

  test "can echo back to one client" do
    {:ok, socket} = :gen_tcp.connect(@ip, @port, mode: :binary, active: false)

    send_data = "hello"
    send_data2 = "world"
    :gen_tcp.send(socket, send_data)
    :gen_tcp.send(socket, send_data2)
    :gen_tcp.shutdown(socket, :write)

    {:ok, received} = :gen_tcp.recv(socket, 0)

    assert received == send_data <> send_data2
  end

  test "can handle multiple clients at once" do
    client_tasks =
      1..10
      |> Enum.map(fn _ ->
        Task.async(fn ->
          {:ok, socket} = :gen_tcp.connect(@ip, @port, mode: :binary, active: false)

          send_data = "hello"
          send_data2 = "world"
          random_data = :crypto.strong_rand_bytes(64)
          :gen_tcp.send(socket, send_data)
          :gen_tcp.send(socket, send_data2)
          :gen_tcp.send(socket, random_data)
          :gen_tcp.shutdown(socket, :write)

          {:ok, received} = :gen_tcp.recv(socket, 0)

          assert received == send_data <> send_data2 <> random_data
        end)
      end)

    Enum.each(client_tasks, &Task.await/1)
  end
end
