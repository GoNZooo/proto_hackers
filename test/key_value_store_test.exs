defmodule KeyValueStoreTest do
  use ExUnit.Case

  @timeout 50
  @max_command_size 1000

  @tag :key_value_store
  test "implements basics of protocol" do
    port = 4201
    {:ok, socket} = :gen_udp.open(0, mode: :binary, active: false)

    # version
    query(socket, port, "version")
    assert_data(socket, "ProtoHackers.ElixirKeyValueStore-0.1")

    # modifying version does nothing
    insert(socket, port, "version", "other-version-string")
    query(socket, port, "version")
    assert_data(socket, "ProtoHackers.ElixirKeyValueStore-0.1")

    # insert
    insert(socket, port, "key", "value")
    query(socket, port, "key")
    assert_data(socket, "key=value")
    insert(socket, port, "key", "other-value")
    query(socket, port, "key")
    assert_data(socket, "key=other-value")

    # absurdly large insert does nothing
    insert(socket, port, "key", String.duplicate("a", @max_command_size))
    query(socket, port, "key")
    assert_data(socket, "key=other-value")
  end

  defp insert(socket, port, key, value) do
    send_to(socket, port, "#{key}=#{value}")
  end

  defp query(socket, port, key) do
    send_to(socket, port, key)
  end

  defp send_to(socket, port, data) do
    :gen_udp.send(socket, {127, 0, 0, 1}, port, data)
  end

  defp assert_data(socket, expected) do
    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, @timeout)
    assert data == expected
  end
end
