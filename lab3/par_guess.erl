-module(par_guess).
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

refine(M) ->
    %io:fwrite("refine start 1~n"),
    NewM =
	refine_rows(
	  transpose(
	    refine_rows(
	      transpose(
		unblocks(
		  refine_rows(
		    blocks(M))))))),
    if M==NewM ->
	    M;
       true ->
	    refine(NewM)
    end.

refine_rows(M) ->
    lists:map(fun refine_row/1, M).
    %KickoffFunc = create_kickoff_func(PoolPid, fun refine_row/1),
    %Nm = lists:map(KickoffFunc, M),
    %io:fwrite("~w~n",[Nm]),
    %T = lists:map(fun await_and_format/1, Nm),
    %io:fwrite("~w~n~n~n",[T]),
    %T.

create_kickoff_func(Pid, Func) ->
    fun(Row) -> Ref = make_ref(),
                Pid ! {ask, self(), Ref},
                receive
                    {ok, Ref, Wp} -> Wp ! {work, self(), Ref, Func, [Row]},
                                {await, Wp, Ref};
                    {no_avail, Ref, _} -> {done, apply(Func, [Row])}
                end
    end.

await_and_format(T) -> 
    case T of
        {done, Result} -> Result;
        {await, Wp, Ref} -> receive 
                                {result, Wp, Ref, Result} -> Result;
                                {error, Wp, Ref} -> %io:fwrite("~w throws in main process ~n",[self()]),
                                    exit(no_solution)
                            end;
        _ -> 'error'
    end.

refine_row(Row) ->
    Entries = entries(Row),
    NewRow =
	[if is_list(X) ->
		 case X--Entries of
		     [] ->
            %io:fwrite("~w refine_row No entries left in a box~n", [self()]),
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
        %io:fwrite("startrow ~w ~w~n", [self(),Row]),
        %io:fwrite("entries ~w ~w~n", [self(),Entries]),
        %io:fwrite("refine_row 2 ~w ~w~n", [self(),NewRow]),
        %io:fwrite("~w refine_row Same entries twice ~n", [self()]),
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

guesses(M) ->
    {I,J,Guesses} = guess(M),
    Ms = [catch refine(update_element(M,I,J,G)) || G <- Guesses],
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

    %io:fwrite("~n~n~w NEW PUZZLE~n~n~n~n~n~n",[self()]),
    %io:fwrite("~w solve start 1~n",[self()]),
    Nm = refine(fill(M)),
    case solved(Nm) of
        true -> Nm;
        false ->
            %io:fwrite("~nCalling par solve guesses!~n"), 
            Solution = par_solve_guesses(PoolPid,guesses(Nm),1),
            %io:fwrite("~w solve end ~n",[self()]),
            case valid_solution(Solution) of
	            true ->
	                Solution;
	            false ->
	                exit({invalid_solution,Solution})
            end
    end.

solve_refined(M) ->
    case solved(M) of
	true ->
        %io:fwrite("~w solve_refined Refinement solved it ~n", [self()]),
	    M;
	false ->
        %io:fwrite("~w solve_refined Not solved start guessing ~n", [self()]),
         %io:fwrite("~w solve_refined refinement wasnt enough, starts guessing ~w ~n", [self(),length(guesses(PoolPid,M))]),
	    solve_one(guesses(M))
    end.

solve_one([]) ->
    %io:fwrite("~w solve_one no more possible solutions~n",[self()]),
    exit(no_solution);
solve_one([M]) ->
    %io:fwrite("~w solve_one last possible guess~n", [self()]),
    solve_refined(M);
solve_one([M|Ms]) ->
    %Ref = make_ref(),
    %io:fwrite("~w solve_one recursive call ~w over ~w possible guesses~n", [self(), Ref, length([M|Ms])]),
    case catch solve_refined(M) of
	{'EXIT',no_solution} ->
        %io:fwrite("~w solve_one failed, trying next one~n", [self()]),
	    solve_one(Ms);
	Solution ->
        %io:fwrite("~w solve_one successful ~n", [self()]),
	    Solution
    end.

%% Parallize solve
%par_solve_guesses(PoolPid,[]) ->
    

par_solve_guesses(PoolPid,G,NbrOfGuessesLeft) ->
    case G of
        [] -> 
            %io:fwrite("~n~w  All work kicked off. Nbr of Guesses : ~w ~n Receiving...~n",[self(),NbrOfGuessesLeft]),
            receive
                {result, _, _, Result} -> 
                        %io:fwrite("~n~w  Got an result ~n",[self()]),
                        PoolPid ! {exit, self()},
                        Result;
                {error, _, _} -> 
                    %io:fwrite("~n~w  Received an no_solution ~n",[self()]),
                    par_solve_guesses(PoolPid,[],NbrOfGuessesLeft-1)
                    %case NbrOfGuessesLeft == 0 of
                    %    true -> 
                    %        io:fwrite("~n~w  Received error from last possible guess ~n FAIL",[self()]),
                    %        exit(no_solution);
                    %    false ->
                    %        io:fwrite("~n~w  Received an no_solution ~n",[self()]),
                    %        par_solve_guesses(PoolPid,[],NbrOfGuessesLeft-1)
                    %end
            end;
        [M|Ms] ->
            %io:fwrite("~n~w  G is : ~w~n",[self(), G]),
            %io:fwrite("~n~w   We have a list!~n",[self()]),
            Ref = make_ref(),
            PoolPid ! {queue, {self(), Ref, fun solve_refined/1,[M]}},
            %io:fwrite("~n~w   We have queued up some work!~n",[self()]),
            par_solve_guesses(PoolPid,Ms,NbrOfGuessesLeft+1)
    end.

%% benchmarks

-define(EXECUTIONS,100).

bm(F) ->
    io:fwrite("~w New Puzzle ~n", [self()]),
    {T,_} = timer:tc(?MODULE,repeat,[F]),
    T/?EXECUTIONS/1000.

repeat(F) ->
    [F() || _ <- lists:seq(1,?EXECUTIONS)].

benchmarks(Puzzles) ->
    PoolPid = pool_start(7),
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

