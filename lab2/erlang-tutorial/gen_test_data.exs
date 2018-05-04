defmodule GenTestData do
    def random_list(interval, n) do
        Enum.map(1..n, fn _ -> :rand.uniform(interval) end) 
    end
end
