defmodule MitmProxyTest do
  use ExUnit.Case

  @tag :mitm
  test "mitm proxy" do
    port = 4209

    # User 1 joins and disconnects because of a bad username (invalid symbols)
    socket = connect(port)
    assert_welcome(socket)
    :gen_tcp.send(socket, "%!@#$%^&*()\n")
    assert_data(socket, "Illegal name!")
    assert_closed(socket)

    # User 1 joins and disconnects because of a bad username (empty name)
    socket = connect(port)
    assert_welcome(socket)
    :gen_tcp.send(socket, " \n")
    assert_data(socket, "Illegal name!")
    assert_closed(socket)

    # User 1 joins
    socket = connect(port)
    assert_welcome(socket)
    :gen_tcp.send(socket, "validUsername1\n")
    assert_presence_message(socket, [])

    # User 2 joins
    socket2 = connect(port)
    assert_welcome(socket2)
    :gen_tcp.send(socket2, "validUsername2\n")
    assert_presence_message(socket2, ["validUsername1"])
    assert_data(socket, "* validUsername2 has joined the room")

    # User 3 joins
    socket3 = connect(port)
    assert_welcome(socket3)
    :gen_tcp.send(socket3, "validUsername3\n")
    assert_presence_message(socket3, ["validUsername1", "validUsername2"])
    assert_data(socket2, "* validUsername3 has joined the room")
    assert_data(socket, "* validUsername3 has joined the room")

    # User 3 sends a message
    legit_address = "7adNeSwJkMakpEcln9HEtthSRtxdmEHOT8T"
    bobs_address = "7YWHMfk9JZe0LM0g1ZauHuiSxhI"

    :gen_tcp.send(socket3, "#{legit_address}\n")
    assert_data(socket2, "[validUsername3] #{bobs_address}")
    assert_data(socket, "[validUsername3] #{bobs_address}")

    # User 3 leaves
    :gen_tcp.close(socket3)
    assert_data(socket2, "* validUsername3 has left the room")
    assert_data(socket, "* validUsername3 has left the room")

    # User 3 joins but leaves before sending username
    socket3 = connect(port)
    assert_welcome(socket3)
    :gen_tcp.close(socket3)
    assert_timeout(socket2, 100)
    assert_timeout(socket, 100)
  end

  defp connect(port) do
    {:ok, socket} =
      :gen_tcp.connect({127, 0, 0, 1}, port, mode: :binary, active: false, packet: :line)

    socket
  end

  defp assert_welcome(socket) do
    {:ok, data} = :gen_tcp.recv(socket, 0, :timer.seconds(2))
    assert data == "Welcome to budgetchat! What shall I call you?\n"
  end

  defp assert_closed(socket) do
    {:error, :closed} = :gen_tcp.recv(socket, 0, :timer.seconds(1))
  end

  defp assert_data(socket, expected) do
    {:ok, data} = :gen_tcp.recv(socket, 0, :timer.seconds(1))
    assert data == expected <> "\n"
  end

  defp assert_presence_message(socket, users) do
    {:ok, data} = :gen_tcp.recv(socket, 0, :timer.seconds(1))

    assert String.starts_with?(data, "* The room contains: ")

    for user <- users do
      assert String.contains?(data, user)
    end
  end

  defp assert_timeout(socket, timeout) do
    {:error, :timeout} = :gen_tcp.recv(socket, 0, timeout)
  end
end
