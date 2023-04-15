defmodule ProtoHackers.ElixirChatProxy.Boguscoin do
  require Logger

  def rewrite(data, target_address) do
    data
    |> String.trim("\n")
    |> String.split(" ")
    |> Enum.map(fn word -> replace_boguscoin_address(word, target_address) end)
    |> Enum.join(" ")
    |> then(fn d -> d <> "\n" end)
  end

  defp replace_boguscoin_address(word, target_address) do
    if boguscoin_address?(word) do
      target_address
    else
      word
    end
  end

  defp boguscoin_address?("7" <> _rest_of_word = word)
       when byte_size(word) >= 26 and byte_size(word) <= 35 do
    alphanumeric?(word)
  end

  defp boguscoin_address?(_word), do: false

  defp alphanumeric?(word) do
    Regex.match?(~r/^[a-zA-Z0-9]+$/, word)
  end
end
