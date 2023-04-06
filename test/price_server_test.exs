defmodule PriceServerTest do
  use ExUnit.Case

  require Logger

  alias ProtoHackers.ElixirPriceServer.Session.Request

  @ports [4204, 4205]

  test "handles one client with the test case from the docs" do
    for port <- @ports do
      Task.async(fn ->
        parameters = [{12345, 101}, {12346, 102}, {12347, 100}, {40960, 5}]

        inserts =
          parameters
          |> Enum.map(fn {timestamp, price} ->
            %Request.Insert{timestamp: timestamp, price: price} |> Request.encode()
          end)

        {:ok, socket} = :gen_tcp.connect({127, 0, 0, 1}, port, mode: :binary, active: false)

        Enum.each(inserts, fn d -> :gen_tcp.send(socket, d) end)

        query = %Request.Query{minimum_time: 12288, maximum_time: 16384} |> Request.encode()
        :gen_tcp.send(socket, query)

        {:ok, <<mean_price::big-integer-size(32)>>} = :gen_tcp.recv(socket, 0)

        :gen_tcp.shutdown(socket, :write)

        assert mean_price == 101
      end)
    end
    |> Enum.each(&Task.await/1)
  end

  test "handles several of the test case from the docs" do
    for port <- @ports,
        _ <- 1..10 do
      Task.async(fn ->
        parameters = [{12345, 101}, {12346, 102}, {12347, 100}, {40960, 5}]

        inserts =
          parameters
          |> Enum.map(fn {timestamp, price} ->
            %Request.Insert{timestamp: timestamp, price: price} |> Request.encode()
          end)

        {:ok, socket} = :gen_tcp.connect({127, 0, 0, 1}, port, mode: :binary, active: false)

        Enum.each(inserts, fn d -> :gen_tcp.send(socket, d) end)

        query = %Request.Query{minimum_time: 12288, maximum_time: 16384} |> Request.encode()
        :gen_tcp.send(socket, query)

        {:ok, <<mean_price::big-integer-size(32)>>} = :gen_tcp.recv(socket, 0)

        :gen_tcp.shutdown(socket, :write)

        assert mean_price == 101
      end)
    end
    |> Enum.each(&Task.await/1)
  end

  @tag timeout: 20_000
  test "handles 200 000 inserts in less than 15 seconds" do
    price = 100

    inserts =
      1..200_000
      |> Enum.map(fn timestamp ->
        %Request.Insert{timestamp: timestamp, price: price} |> Request.encode()
      end)

    start_time = System.monotonic_time(:millisecond)

    @ports
    |> Enum.map(fn port ->
      Task.async(fn ->
        internal_start_time = System.monotonic_time(:millisecond)

        {:ok, socket} = :gen_tcp.connect({127, 0, 0, 1}, port, mode: :binary, active: false)

        Enum.each(inserts, fn d -> :gen_tcp.send(socket, d) end)

        :gen_tcp.send(
          socket,
          Request.encode(%Request.Query{minimum_time: 1, maximum_time: 200_000})
        )

        {:ok, <<mean_price::big-integer-size(32)>>} = :gen_tcp.recv(socket, 0)
        assert mean_price == price
        :gen_tcp.shutdown(socket, :write)

        internal_end_time = System.monotonic_time(:millisecond)
        diff = internal_end_time - internal_start_time
        Logger.debug("Took #{diff} ms to insert 200 000 prices for port #{port}")
      end)
    end)
    |> Enum.each(fn t -> Task.await(t, 15_000) end)

    end_time = System.monotonic_time(:millisecond)

    assert end_time - start_time < 15_000
  end
end
