defmodule Helpers do
    def random_list(interval, n) do
        Enum.map(1..n, fn _ -> :rand.uniform(interval) end) 
    end

    def benchmark(n, module, fun, fun_input) do
        runs = for i <- 0..n, i > 0, do: 
            :timer.tc(module, fun, [fun_input]) 
            |> elem(0)
        Enum.sum(runs) / length(runs)
    end
end
