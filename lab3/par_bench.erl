-module(par_bench).
-import(sudoku, [solve/1]).
-import(pool, [pool_start/1]).
-compile(export_all).

%% Runs the benchmark in parallel by splitting it onto a worker pool.
par_benchmarks(Puzzles) -> 
    Pp = pool_start(10),
    solver(Pp, Puzzles).

solver(_, []) -> [];
solver(Pool, [P|Puzzles]) ->
    Pool ! {ask, self()},
    {Name, M} = P,
    receive
        {ok, Wp} -> Wp ! {work, self(), fun() -> solve(M) end, []},
                    receive {result, _, R} ->
                        [{Name, R}] ++ solver(Pool, Puzzles)
                    end
        %{no_avail, _} ->  
    end.





%benchmarks(Puzzles) ->
%    [{Name,bm(fun()->solve(M) end)} || {Name,M} <- Puzzles].

benchmarks() ->
  {ok,Puzzles} = file:consult("problems.txt"),
  timer:tc(?MODULE,par_benchmarks,[Puzzles]).
