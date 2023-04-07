defmodule ChatServerTest do
  use ExUnit.Case

  require Logger

  alias ProtoHackers.ElixirChatServer.Presence

  @ports [4207]

  test "user is asked for their username and socket disconnects if given invalid username" do
    port = 4207

    # User 1 joins and disconnects because of a bad username
    socket = connect(port)
    assert_welcome(socket)
    :gen_tcp.send(socket, "%!@#$%^&*()\n")
    assert_closed(socket)

    # User 1 joins
    socket = connect(port)
    assert_welcome(socket)
    :gen_tcp.send(socket, "validUsername1\n")
    assert_data(socket, "* Users: ")
    assert_users(["validUsername1"])

    # User 2 joins
    socket2 = connect(port)
    assert_welcome(socket2)
    :gen_tcp.send(socket2, "validUsername2\n")
    assert_data(socket2, "* Users: validUsername1")
    assert_data(socket, "* validUsername2 has joined")

    # User 3 joins
    socket3 = connect(port)
    assert_welcome(socket3)
    :gen_tcp.send(socket3, "validUsername3\n")
    assert_data(socket3, "* Users: validUsername1, validUsername2")
    assert_data(socket2, "* validUsername3 has joined")
    assert_data(socket, "* validUsername3 has joined")

    # User 3 sends a message
    :gen_tcp.send(socket3, "Hello!\n")
    assert_data(socket2, "[validUsername3] Hello!")
    assert_data(socket, "[validUsername3] Hello!")

    # User 3 leaves
    :gen_tcp.close(socket3)
    assert_data(socket2, "* validUsername3 has left")
    assert_data(socket, "* validUsername3 has left")
  end

  defp connect(port) do
    {:ok, socket} =
      :gen_tcp.connect({127, 0, 0, 1}, port, mode: :binary, active: false, packet: :line)

    socket
  end

  defp assert_welcome(socket) do
    {:ok, data} = :gen_tcp.recv(socket, 0)
    assert data == "Welcome! What is your username?\n"
  end

  defp assert_closed(socket) do
    {:error, :closed} = :gen_tcp.recv(socket, 0)
  end

  defp assert_data(socket, expected) do
    {:ok, data} = :gen_tcp.recv(socket, 0)
    assert data == expected <> "\n"
  end

  defp assert_users(users) do
    assert Presence.get_users() == users
  end
end
