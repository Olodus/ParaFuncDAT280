-module(par_bench).
-import(sudoku, [solve/1]).
-import(pool, [pool_start/1]).
-compile(export_all).

%% Runs the benchmark in parallel by splitting it onto a worker pool.
par_benchmarks(Puzzles) -> 
    Pp = pool_start(2),
    solver(Pp, Puzzles).

solver(_, []) -> [];
solver(Pool, [P|Puzzles]) ->
    Ref = make_ref(),
    Pool ! {ask, self(), Ref},
    {Name, M} = P,
    receive
        {ok, Ref, Wp} -> Wp ! {work, self(), Ref, fun() -> sudoku:bm(fun()-> solve(M) end) end, []},
                    receive {result, _, Ref, R} ->
                        [{Name, R}] ++ solver(Pool, Puzzles)
                    end
        %{no_avail, _} ->  
    end.

benchmarks() ->
  {ok,Puzzles} = file:consult("problems.txt"),
  timer:tc(?MODULE,par_benchmarks,[Puzzles]).
