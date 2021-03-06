%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is a very simple implementation of map-reduce, in both 
%% sequential and parallel versions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(map_reduce).
-compile(export_all).
-import(pool,[pool_start/1]).
%% We begin with a simple sequential implementation, just to define
%% the semantics of map-reduce. 

%% The input is a collection of key-value pairs. The map function maps
%% each key value pair to a list of key-value pairs. The reduce
%% function is then applied to each key and list of corresponding
%% values, and generates in turn a list of key-value pairs. These are
%% the result.

%% Sequential implementation

map_reduce_seq(Map,Reduce,Input) ->
    Mapped = [{K2,V2}
	      || {K,V} <- Input,
		 {K2,V2} <- Map(K,V)],
    reduce_seq(Reduce,Mapped).

reduce_seq(Reduce,KVs) ->
    [KV || {K,Vs} <- group(lists:sort(KVs)),
	   KV <- Reduce(K,Vs)].

group([]) ->
    [];
group([{K,V}|Rest]) ->
    group(K,[V],Rest).

group(K,Vs,[{K,V}|Rest]) ->
    group(K,[V|Vs],Rest);
group(K,Vs,Rest) ->
    [{K,lists:reverse(Vs)}|group(Rest)].

split_into(N,L) ->
    split_into(N,L,length(L)).

split_into(1,L,_) ->
    [L];
split_into(N,L,Len) ->
    {Pre,Suf} = lists:split(Len div N,L),
    [Pre|split_into(N-1,Suf,Len-(Len div N))].

%% Parallel implementation

map_reduce_par(Map,M,Reduce,R,Input) ->
    Parent = self(),
    Splits = split_into(M,Input),
    Mappers = 
	[spawn_mapper_par(Parent,Map,R,Split)
	 || Split <- Splits],
    Mappeds = 
	[receive {Pid,L} -> L end || Pid <- Mappers],
    Reducers = 
	[spawn_reducer_par(Parent,Reduce,I,Mappeds) 
	 || I <- lists:seq(0,R-1)],
    Reduceds = 
	[receive {Pid,L} -> L end || Pid <- Reducers],
    lists:sort(lists:flatten(Reduceds)).

spawn_mapper_par(Parent,Map,R,Split) ->
    spawn_link(fun() ->
			Mapped = [{erlang:phash2(K2,R),{K2,V2}}
				  || {K,V} <- Split,
				     {K2,V2} <- Map(K,V)],
			Parent ! {self(),group(lists:sort(Mapped))}
		end).

spawn_reducer_par(Parent,Reduce,I,Mappeds) ->
    Inputs = [KV
	      || Mapped <- Mappeds,
		 {J,KVs} <- Mapped,
		 I==J,
		 KV <- KVs],
    spawn_link(fun() -> Parent ! {self(),reduce_seq(Reduce,Inputs)} end).

%% Distributed implementation

map_reduce_dis(Map,M,Reduce,R,Input) ->
    Parent = self(),
    Splits = split_into(M,Input),
    Nodes = nodes() ++ [node()],

    MapWork = divide_work(Splits, Nodes),
    
    Mappers = 
	[spawn_mapper_dis(Parent,Map,R,Split, Node)
	|| {Split, Node} <- MapWork],
  
    Mappeds = 
	[receive {Pid,L} -> L end || Pid <- Mappers],

    RedWork = divide_work(lists:seq(0,R-1), Nodes),

    Reducers = 
	[spawn_reducer_dis(Parent,Reduce,I,Mappeds, Node) 
	 || {I, Node} <- RedWork],
    Reduceds = 
	[receive {Pid,L} -> L end || Pid <- Reducers],
    lists:sort(lists:flatten(Reduceds)).

spawn_mapper_dis(Parent,Map,R,Split, Node) ->
    spawn_link(Node, fun() ->
                 
                {ok,web} = dets:open_file(web,[{file,"web.dat"}]),
                Mapped = [{erlang:phash2(K2,R),{K2,V2}}
				  || {K,V} <- Split,
				     {K2,V2} <- Map(K,V)],

			Parent ! {self(),group(lists:sort(Mapped))}
		end).

spawn_reducer_dis(Parent,Reduce,I,Mappeds, Node) ->
    Inputs = [KV
	      || Mapped <- Mappeds,
		 {J,KVs} <- Mapped,
		 I==J,
		 KV <- KVs],
    spawn_link(Node, fun() -> Parent ! {self(),reduce_seq(Reduce,Inputs)} end).

%% Balanced Distributed implementation


map_reduce_dis_bal(PoolPids, Map, M, Reduce, R, Input) ->
    Parent = self(),
    Splits = split_into(M,Input),

    Work = divide_work(Splits, PoolPids),
    MapFunc = fun({MW, Pp}) ->
                        Ref = make_ref(),
                        Pp ! {queue, {Parent, Ref, fun map_bal/3, [Map, R, MW]}},
                        {Pp, Ref, MW}  end,
    Mappers = lists:map(MapFunc, Work),
    
    Mappeds = receive_result(Mappers, MapFunc, PoolPids),

    P1 = lists:last(Mappeds),
    M1 = lists:droplast(Mappeds),

    RedWork = divide_work(lists:seq(0,R-1), P1),
    RedFunc = fun({RW, Pp}) ->
                       Ref = make_ref(),
                       Pp ! {queue, {Parent, Ref, fun red_bal/3, [Reduce, RW, M1]}},
                       {Pp, Ref, RW} end,
    Reducers = lists:map(RedFunc, RedWork),
   
    Reduceds = receive_result(Reducers, RedFunc, P1),

    lists:sort(lists:flatten(lists:droplast(Reduceds))).

map_reduce_dis_bal_new(PoolPids, Map, M, Reduce, R, Input) ->
    Parent = self(),
    Splits = split_into(M,Input),

    Work = divide_work(Splits, PoolPids),
    MapFunc = fun({MW, Pp}) ->
                        Ref = make_ref(),
                        Pp ! {ask_and_wait, Parent, Ref},
                        {Pp, Ref, [Map, R, MW]}  end,
    Mappers = lists:map(MapFunc, Work),
    
    Mappeds = receive_result_new(Mappers, MapFunc, fun map_bal/3, PoolPids),

    P1 = lists:last(Mappeds),
    M1 = lists:droplast(Mappeds),

    RedWork = divide_work(lists:seq(0,R-1), P1),
    RedFunc = fun({RW, Pp}) ->
                       Ref = make_ref(),
                       Pp ! {ask_and_wait, Parent, Ref},
                       {Pp, Ref, [Reduce, RW, M1]} end,
    Reducers = lists:map(RedFunc, RedWork),
   
    Reduceds = receive_result_new(Reducers, RedFunc, fun red_bal/3, P1),

    lists:sort(lists:flatten(lists:droplast(Reduceds))).


receive_result_new([], _, _, PoolPids) -> [PoolPids];
receive_result_new(Work, SendFunc, WorkFunc, PoolPids) ->
    receive
        {ok, Ref, Wp} ->
            UnsentWork = lists:find(fun({_,R,_}) -> Ref == R end, Work),
            Wp ! {work, self(), Ref, WorkFunc, element(3,UnsentWork)},
            receive_result_new(lists:delete(UnsentWork,Work),SendFunc,WorkFunc,PoolPids);
        {result, _, Ref, Result} ->
            F = fun(T) -> element(2,T) /= Ref end,
            Filtered_work = 
            [Result]++receive_result(lists:filter(F, Work), SendFunc, PoolPids);
        {'EXIT', Node, _} ->
            F2 = fun({Pid, _, _}) -> is_pid_on_node(Pid,Node) end,
            CrashedWork = lists:filter(F2, Work),
            F3 = fun({Pid, _, _}) -> not is_pid_on_node(Pid,Node) end,
            NotCrashedWork = lists:filter(F3, Work),
            CWork = lists:map(fun({_,_,W}) -> W end, CrashedWork),
            NewPool = lists:filter(fun(X) -> not is_pid_on_node(X,Node) end,PoolPids),
            DivW = divide_work(CWork, NewPool),
            ResentWork = lists:map(SendFunc, DivW),
            receive_result(ResentWork++NotCrashedWork, SendFunc, NewPool)
    end.



receive_result([], _, PoolPids) -> [PoolPids];
receive_result(Work, SendFunc, PoolPids) ->
    receive
        {result, _, Ref, Result} ->
            F = fun(T) -> element(2,T) /= Ref end,
            Filtered_work = 
            [Result]++receive_result(lists:filter(F, Work), SendFunc, PoolPids);
        {'EXIT', Node, _} ->
            F2 = fun({Pid, Ref, Wo}) -> is_pid_on_node(Pid,Node) end,
            CrashedWork = lists:filter(F2, Work),
            F3 = fun({Pid, Ref, Wo}) -> not is_pid_on_node(Pid,Node) end,
            NotCrashedWork = lists:filter(F3, Work),
            CWork = lists:map(fun({_,_,W}) -> W end, CrashedWork),
            NewPool = lists:filter(fun(X) -> not is_pid_on_node(X,Node) end,PoolPids),
            DivW = divide_work(CWork, NewPool),
            ResentWork = lists:map(SendFunc, DivW),
            receive_result(ResentWork++NotCrashedWork, SendFunc, NewPool)
    end.

is_pid_on_node(Pid,Node) ->
    node(Pid) == node(Node).



map_bal(Map, R, Split) ->
    {ok,web} = dets:open_file(web,[{file,"web.dat"}]),
    Mapped = [{erlang:phash2(K2,R),{K2,V2}}                   
            || {K,V} <- Split,
            {K2,V2} <- Map(K,V)],
    group(lists:sort(Mapped)).

red_bal(Reduce, I, Mappeds) ->
    Inputs = [KV
	      || Mapped <- Mappeds,
		 {J,KVs} <- Mapped,
		 I==J,
		 KV <- KVs],
    reduce_seq(Reduce,Inputs).


%% Helper functions

%% Helper functions to divide work between distributed nodes
divide_work(Work, Nodes) -> 
   divide_work(Work, Nodes, Nodes).

divide_work([], _, _) -> [];
divide_work(Work, Nodes, []) -> divide_work(Work, Nodes, Nodes);
divide_work([W|Work], Nodes, [Node|Tail]) ->
    [{W, Node}] ++ divide_work(Work, Nodes, Tail).

