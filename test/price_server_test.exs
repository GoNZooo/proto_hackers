defmodule PriceServerTest do
  use ExUnit.Case

  alias ProtoHackers.ElixirPriceServer.Session.Request

  @ports [4205]

  test "handles the test case from the docs" do
    @ports
    |> Enum.map(fn port ->
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

        assert mean_price == 101
      end)
    end)
    |> Enum.each(&Task.await/1)
  end
end
