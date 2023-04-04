defmodule ProtoHackers.Prime do
  def prime?(2), do: true
  def prime?(3), do: true
  def prime?(n) when n < 0, do: false

  def prime?(n) do
    upper_bound = n |> :math.sqrt() |> trunc()

    Enum.all?(2..upper_bound, fn x -> rem(n, x) != 0 end)
  end
end
