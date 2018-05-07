defmodule ErlPsort do
    alias SeqMergeSort, as: Merge

    def start(x,depth) do
      erl_psort(self(),x,depth)
      receive do
        x -> x
      end
    end
    
    def erl_psort(pid,[], _), do: send pid, []
    def erl_psort(pid, x, depth) do
        len = length(x)
        if len > 1 do
            {l1, l2} = Enum.split(x, round(len/2)) 
            if depth > 0 do
                spawn_link(ErlPsort, :erl_psort, [self(),l1,depth-1])
                spawn_link(ErlPsort, :erl_psort, [self(),l2,depth-1])
                receive do 
                  x -> h1 = x
                end
                receive do
                  y -> send pid, Merge.merge(y,h1)
                end
            else
                send pid, Merge.merge(Merge.seq_merge_sort(l1), Merge.seq_merge_sort(l2))
            end
        else
            send pid, x
        end
    end 

end
