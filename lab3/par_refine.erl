-module(par_refine).
%-include_lib("eqc/include/eqc.hrl").
-import(pool,[pool_start/1]).
-compile(export_all).

%% %% generators

%% matrix(M,N) ->
%%     vector(M,vector(N,nat())).

%% matrix transpose

transpose([Row]) ->
    [[X] || X <- Row];
transpose([Row|M]) ->
    [[X|Xs] || {X,Xs} <- lists:zip(Row,transpose(M))].

%% prop_transpose() ->
%%     ?FORALL({M,N},{nat(),nat()},
%% 	    ?FORALL(Mat,matrix(M+1,N+1),
%% 		    transpose(transpose(Mat)) == Mat)).

%% map a matrix to a list of 3x3 blocks, each represented by the list
%% of elements in row order

triples([A,B,C|D]) ->
    [[A,B,C]|triples(D)];
triples([]) ->
    [].

blocks(M) ->
    Blocks = [triples(X) || X <- transpose([triples(Row) || Row <- M])],
    lists:append(
      lists:map(fun(X)->
			lists:map(fun lists:append/1, X)
		end,
		Blocks)).

unblocks(M) ->
    lists:map(
      fun lists:append/1,
      transpose(
	lists:map(
	  fun lists:append/1,
	  lists:map(
	    fun(X)->lists:map(fun triples/1,X) end,
	    triples(M))))).

%% prop_blocks() ->
%%     ?FORALL(M,matrix(9,9),
%% 	    unblocks(blocks(M)) == M).

%% decide whether a position is safe

entries(Row) ->
    [X || X <- Row,
	  1 =< X andalso X =< 9].

safe_entries(Row) ->
    Entries = entries(Row),
    lists:sort(Entries) == lists:usort(Entries).

safe_rows(M) ->
    lists:all(fun safe_entries/1,M).

safe(M) ->
    safe_rows(M) andalso
	safe_rows(transpose(M)) andalso
	safe_rows(blocks(M)).

%% fill blank entries with a list of all possible values 1..9

fill(M) ->
    Nine = lists:seq(1,9),
    [[if 1=<X, X=<9 ->
	      X;
	 true ->
	      Nine
      end
      || X <- Row]
     || Row <- M].

%% refine entries which are lists by removing numbers they are known
%% not to be

refine(PoolPid,M) ->
    io:fwrite("refine start 1~n"),
    NewM =
	refine_rows(PoolPid,
	  transpose(
	    refine_rows(PoolPid,
	      transpose(
		unblocks(
		  refine_rows(PoolPid,
		    blocks(M))))))),
    if M==NewM ->
	    M;
       true ->
	    refine(PoolPid,NewM)
    end.

refine_rows(PoolPid,M) ->
    KickoffFunc = create_kickoff_func(PoolPid),
    Nm = lists:map(KickoffFunc, M),
    io:fwrite("~w~n",[Nm]),
    T = lists:map(fun await_and_format/1, Nm),
    io:fwrite("~w~n",[T]),
    T.

create_kickoff_func(Pid) ->
    fun(Row) -> Ref = make_ref(),
                Pid ! {ask, self(), Ref},
                receive
                    {ok, Ref, Wp} -> Wp ! {work, self(), Ref, fun refine_row/1, [Row]},
                                {await, Wp, Ref};
                    {no_avail, Ref, _} -> {done, apply(fun refine_row/1, [Row])}
                end
    end.

await_and_format(T) -> 
    case T of
        {done, Result} -> Result;
        {await, Wp, Ref} -> receive {result, Wp, Ref, Result} -> Result end;
        _ -> 'error'
    end.

refine_row(Row) ->
    Entries = entries(Row),
    NewRow =
	[if is_list(X) ->
		 case X--Entries of
		     [] ->
            io:fwrite("refine_row 1~n"),
			 exit(no_solution);
		     [Y] ->
			 Y;
		     NewX ->
			 NewX
		 end;
	    true ->
		 X
	 end
	 || X <- Row],
    NewEntries = entries(NewRow),
    %% check we didn't create a duplicate entry
    case length(lists:usort(NewEntries)) == length(NewEntries) of
	true ->
	    NewRow;
	false ->
        io:fwrite("startrow ~w ~w~n", [self(),Row]),
        io:fwrite("entries ~w ~w~n", [self(),Entries]),
        io:fwrite("refine_row 2 ~w ~w~n", [self(),NewRow]),
	    exit(no_solution)
    end.

is_exit({'EXIT',_}) ->
    true;
is_exit(_) ->
    false.

%% is a puzzle solved?

solved(M) ->
    lists:all(fun solved_row/1,M).

solved_row(Row) ->
    lists:all(fun(X)-> 1=<X andalso X=<9 end, Row).

%% how hard is the puzzle?

hard(M) ->		      
    lists:sum(
      [lists:sum(
	 [if is_list(X) ->
		  length(X);
	     true ->
		  0
	  end
	  || X <- Row])
       || Row <- M]).

%% choose a position {I,J,Guesses} to guess an element, with the
%% fewest possible choices

guess(M) ->
    Nine = lists:seq(1,9),
    {_,I,J,X} =
	lists:min([{length(X),I,J,X}
		   || {I,Row} <- lists:zip(Nine,M),
		      {J,X} <- lists:zip(Nine,Row),
		      is_list(X)]),
    {I,J,X}.

%% given a matrix, guess an element to form a list of possible
%% extended matrices, easiest problem first.

guesses(PoolPid,M) ->
    {I,J,Guesses} = guess(M),
    Ms = [catch refine(PoolPid,update_element(M,I,J,G)) || G <- Guesses],
    SortedGuesses =
	lists:sort(
	  [{hard(NewM),NewM}
	   || NewM <- Ms,
	      not is_exit(NewM)]),
    [G || {_,G} <- SortedGuesses].

update_element(M,I,J,G) ->
    update_nth(I,update_nth(J,G,lists:nth(I,M)),M).

update_nth(I,X,Xs) ->
    {Pre,[_|Post]} = lists:split(I-1,Xs),
    Pre++[X|Post].

%% prop_update() ->
%%     ?FORALL(L,list(int()),
%% 	    ?IMPLIES(L/=[],
%% 		     ?FORALL(I,choose(1,length(L)),
%% 			     update_nth(I,lists:nth(I,L),L) == L))).

%% solve a puzzle

solve(PoolPid,M) ->
    io:fwrite("solve start 1~n"),
    Solution = solve_refined(PoolPid,refine(PoolPid,fill(M))),
    case valid_solution(Solution) of
	true ->
	    Solution;
	false ->
	    exit({invalid_solution,Solution})
    end.

solve_refined(PoolPid,M) ->
    case solved(M) of
	true ->
	    M;
	false ->
	    solve_one(PoolPid,guesses(PoolPid,M))
    end.

solve_one(PoolPid,[]) ->
    io:fwrite("solve_one~n"),
    exit(no_solution);
solve_one(PoolPid,[M]) ->
    solve_refined(PoolPid,M);
solve_one(PoolPid,[M|Ms]) ->
    case catch solve_refined(PoolPid,M) of
	{'EXIT',no_solution} ->
	    solve_one(PoolPid,Ms);
	Solution ->
	    Solution
    end.

%% benchmarks

-define(EXECUTIONS,100).

bm(F) ->
    {T,_} = timer:tc(?MODULE,repeat,[F]),
    T/?EXECUTIONS/1000.

repeat(F) ->
    [F() || _ <- lists:seq(1,?EXECUTIONS)].

benchmarks(Puzzles) ->
    PoolPid = pool_start(2),
    [{Name,bm(fun()->solve(PoolPid,M) end)} || {Name,M} <- Puzzles].

benchmarks() ->
    {ok,Puzzles} = file:consult("problems.txt"),
    timer:tc(?MODULE,benchmarks,[Puzzles]).
		      
%% check solutions for validity

valid_rows(M) ->
    lists:all(fun valid_row/1,M).

valid_row(Row) ->
    lists:usort(Row) == lists:seq(1,9).

valid_solution(M) ->
    valid_rows(M) andalso valid_rows(transpose(M)) andalso valid_rows(blocks(M)).

