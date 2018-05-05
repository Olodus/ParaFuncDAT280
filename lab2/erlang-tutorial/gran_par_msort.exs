defmodule GranParMergeSort do
    alias SeqMergeSort, as: Merge
    
    def gran_par_merge_sort([], _), do: []
    def gran_par_merge_sort(x, depth) do
        len = length(x)
        if len > 1 do
            {l1, l2} = Enum.split(x, round(len/2)) 
            if depth > 0 do
                Merge.merge(
                    Task.await(Task.async(GranParMergeSort, :gran_par_merge_sort, [l1, depth - 1])),              
                    Task.await(Task.async(GranParMergeSort, :gran_par_merge_sort, [l2, depth - 1])))
            else
                    Merge.merge(gran_par_merge_sort(l1, 0), gran_par_merge_sort(l2, 0))
            end
        else
            x
        end
    end 
end
