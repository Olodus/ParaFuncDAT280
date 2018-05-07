defmodule Qsort do
    def random_list(interval, n) do
        Enum.map(1..n, fn _ -> :rand.uniform(interval) end)
    end

    def benchmark(n, module, fun, fun_input) do
        runs = for i <- 0..n, i > 0, do:
            :timer.tc(module, fun, fun_input)  
            |> elem(0)
        Enum.sum(runs) / length(runs)
    end 

    def qsort([]),      do: []
    def qsort([x|xs])   do
        qsort(Enum.filter(xs, fn n -> n < x end))
        ++ [x]
        ++ qsort(Enum.filter(xs, fn n -> n >= x end))
    end

    def parqsort([]),      do: []
    def parqsort([x|xs])   do
        parqsort(Enum.filter(xs, fn n -> n < x end))
        ++ [x]
        ++ Task.await(Task.async(Qsort, :parqsort, [(Enum.filter(xs, fn n -> n >= x end))]))
    end

    def granparqsort(0, xs),    do: qsort(xs)
    def granparqsort(_, []),    do: []
    def granparqsort(d, [x|xs]) do
        granparqsort(d-1, Enum.filter(xs, fn n -> n < x end))
        ++ [x]
        ++ Task.await(Task.async(Qsort, :granparqsort, [d-1, (Enum.filter(xs, fn n -> n >= x end))]))
    end
end
