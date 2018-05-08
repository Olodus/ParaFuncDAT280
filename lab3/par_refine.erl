
%% Runs the refinement in parallel by splitting it onto the 
par_refine(PoolPid, M) ->
    NewM =
    par_refine_rows(PoolPid,
      transpose(
        par_refine_rows(PoolPid,
          transpose(
        unblocks(
          par_refine_rows(PoolPid,
            blocks(M))))))),
    if M==NewM ->
           M;
       true ->
           refine(NewM)
    end.

par_refine_rows(PoolPid,M) ->
    Nm = lists:map(fun(PoolPid,X) -> apply(para,[PoolPid,X]) end, M),
    lists:map(fun await_and_format/1, Nm).

para(PoolPid,Row) ->
    PoolPid ! {ask, self()},
    receive
        {ok, Wp} -> Wp ! {work, self(), fun refine_row/1, [Row]},
                    {await, Wp}.
        {no_avail, _} -> {done, apply(refine_row/1, [Row])}.

await_and_format(T) -> 
    case T of
        {done, Result} -> Result;
        {await, Wp} -> receive {result, Wp, Result} -> Result
        _ -> 'error'
    end.
