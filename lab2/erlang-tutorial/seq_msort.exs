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

    def seq_merge_sort([]), do: []
    def seq_merge_sort(x) do
        len = length(x)
        if len > 1 do
            {l1, l2} = Enum.split(x, round(len/2))
            merge(seq_merge_sort(l1), seq_merge_sort(l2))
        else
            x
        end
    end 
end
