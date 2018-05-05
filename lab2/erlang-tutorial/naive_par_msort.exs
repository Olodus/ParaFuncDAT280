defmodule NaiveParMergeSort do
    alias SeqMergeSort, as: Merge
    
    def naive_par_merge_sort([]), do: []
    def naive_par_merge_sort(x) do
        len = length(x)
        if len > 1 do
            {l1, l2} = Enum.split(x, round(len/2)) 
            Merge.merge(
                Task.await(Task.async(NaiveParMergeSort, :naive_par_merge_sort, [l1])),              
                Task.await(Task.async(NaiveParMergeSort, :naive_par_merge_sort, [l2])))
        else
            x
        end
    end 
end
