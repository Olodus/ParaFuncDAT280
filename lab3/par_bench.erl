-module(par_bench).
-import(sudoku, [solve/1]).
-import(pool, [pool_start/1]).
-compile(export_all).

%% Runs the benchmark in parallel by splitting it onto a worker pool.
par_benchmarks(Puzzles) -> 
    Pp = pool_start(8),
    Kf = create_kickoff_func(Pp, fun solve_one/1),
    Nm = lists:map(Kf, Puzzles),
    T = lists:map(fun await_and_format/1, Nm),
    T.

solve_one(Puzzle) ->
    {Name, M} = Puzzle,
    Res = sudoku:bm(fun() -> solve(M) end),
    {Name,Res}.


create_kickoff_func(Pid, Func) ->
    fun(Puzzle) -> 
                Ref = make_ref(),
                Pid ! {ask, self(), Ref},
                receive
                    {ok, Ref, Wp} -> 
                            Wp ! {work, self(), Ref, Func, [Puzzle]},
                            {await, Wp, Ref};
                    {no_avail, Ref, _} -> {done, apply(Func, [Puzzle])}
                end
    end.

await_and_format(T) ->
    case T of
        {done, Result} -> Result;
        {await, Wp, Ref} -> receive
                                {result, Wp, Ref, Result} -> Result;
                                {error, Wp, Ref} -> exit(no_solution)
                            end;
        _ -> 'error'
    end.


benchmarks() ->
  {ok,Puzzles} = file:consult("problems.txt"),
  timer:tc(?MODULE,par_benchmarks,[Puzzles]).
