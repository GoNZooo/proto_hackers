defmodule ElixirPrimeServerTest do
  use ExUnit.Case

  alias ProtoHackers.Prime

  @ip {127, 0, 0, 1}
  @port 4203

  test "can check prime numbers" do
    primes = [
      2,
      3,
      5,
      7,
      11,
      13,
      17,
      19,
      23,
      29,
      31,
      37,
      41,
      43,
      47,
      53,
      59,
      61,
      67,
      71,
      73,
      79,
      83,
      89
    ]

    non_primes = [4, 6, 8, 9, 10, 12, 14, 15, 16, 18]

    assert Enum.all?(primes, &Prime.prime?/1)
    assert Enum.all?(non_primes, fn x -> not Prime.prime?(x) end)
  end

  test "responds to just one client with two requests" do
    {:ok, socket} = :gen_tcp.connect(@ip, @port, mode: :binary, active: false)

    send_data = (%{method: "isPrime", number: 89} |> Jason.encode!()) <> "\n"
    :gen_tcp.send(socket, send_data)
    {:ok, received} = :gen_tcp.recv(socket, 0)
    as_json = Jason.decode!(received)
    assert as_json == %{"method" => "isPrime", "prime" => true}

    send_data = (%{method: "isPrime", number: 90} |> Jason.encode!()) <> "\n"
    :gen_tcp.send(socket, send_data)
    {:ok, received} = :gen_tcp.recv(socket, 0)
    as_json = Jason.decode!(received)
    assert as_json == %{"method" => "isPrime", "prime" => false}

    :gen_tcp.shutdown(socket, :write)
  end

  test "rejects non-json payloads" do
    {:ok, socket} = :gen_tcp.connect(@ip, @port, mode: :binary, active: false)

    send_data = "not json"
    :gen_tcp.send(socket, send_data)
    {:ok, received} = :gen_tcp.recv(socket, 0)
    assert received == "malformed request"
    assert :gen_tcp.recv(socket, 0) == {:error, :closed}

    :gen_tcp.shutdown(socket, :write)
  end

  test "rejects payloads missing keys" do
    {:ok, socket} = :gen_tcp.connect(@ip, @port, mode: :binary, active: false)

    send_data = (%{method: "isPrime"} |> Jason.encode!()) <> "\n"
    :gen_tcp.send(socket, send_data)
    {:ok, received} = :gen_tcp.recv(socket, 0)
    assert received == "malformed request"
    assert :gen_tcp.recv(socket, 0) == {:error, :closed}

    :gen_tcp.shutdown(socket, :write)
  end

  test "identifies floats as non-prime" do
    {:ok, socket} = :gen_tcp.connect(@ip, @port, mode: :binary, active: false)

    send_data = (%{method: "isPrime", number: 89.0} |> Jason.encode!()) <> "\n"
    :gen_tcp.send(socket, send_data)
    {:ok, received} = :gen_tcp.recv(socket, 0)
    as_json = Jason.decode!(received)
    assert as_json == %{"method" => "isPrime", "prime" => false}

    :gen_tcp.shutdown(socket, :write)
  end

  test "identifies negative numbers as non-prime" do
    {:ok, socket} = :gen_tcp.connect(@ip, @port, mode: :binary, active: false)

    send_data = (%{method: "isPrime", number: -2} |> Jason.encode!()) <> "\n"
    :gen_tcp.send(socket, send_data)
    {:ok, received} = :gen_tcp.recv(socket, 0)
    as_json = Jason.decode!(received)
    assert as_json == %{"method" => "isPrime", "prime" => false}

    :gen_tcp.shutdown(socket, :write)
  end

  test "can handle multiple clients at once" do
    client_tasks =
      1..10
      |> Enum.map(fn _ ->
        Task.async(fn ->
          {:ok, socket} = :gen_tcp.connect(@ip, @port, mode: :binary, active: false)

          send_data = (%{method: "isPrime", number: 89} |> Jason.encode!()) <> "\n"
          :gen_tcp.send(socket, send_data)
          {:ok, received} = :gen_tcp.recv(socket, 0)
          as_json = Jason.decode!(received)
          assert as_json == %{"method" => "isPrime", "prime" => true}

          send_data = (%{method: "isPrime", number: 90} |> Jason.encode!()) <> "\n"
          :gen_tcp.send(socket, send_data)
          {:ok, received} = :gen_tcp.recv(socket, 0)
          as_json = Jason.decode!(received)
          assert as_json == %{"method" => "isPrime", "prime" => false}

          :gen_tcp.shutdown(socket, :write)
        end)
      end)

    Enum.each(client_tasks, &Task.await/1)
  end
end
