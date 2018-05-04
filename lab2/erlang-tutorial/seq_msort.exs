defmodule SeqMergeSort do
    def merge([], y),           do: y
    def merge(x, []),           do: x
    def merge([x|xs], [y|ys])   do
        if x < y do
            [x|merge(xs, [y|ys])]
        else
            [y|merge(ys, [x|xs])]
        end
    end
end
