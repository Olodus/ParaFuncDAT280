-module(pool).
-compile(export_all).

%% returns the pid of the pool manager
pool_start(Nbr_workers) ->
    Nodes = nodes(),
    lists:map(fun(Node) -> pool_init(Node, Nbr_workers) end, Nodes).

pool_init(Node, Nbr_workers) -> 
    Pool_pid = spawn_link(Node, pool,pool_loop,[[],[]]),
    Ws = start_workers(Node, Nbr_workers, Pool_pid),
    Pool_pid ! {new_ws, Ws},
    Pool_pid.

start_workers(_, 0, _) -> [];
start_workers(Node, X, Pp) -> [spawn_link(Node, pool, worker, [Pp]) | start_workers(Node, X-1, Pp)].

%% Waits for messages from workgivers or workers finished with some work.
%% Calls itself
pool_loop([],W) ->
    receive 
        {ask, Pid, Ref} -> 
            Pid ! {no_avail, Ref, 0}, 
            pool_loop([],W);
        {available, Pid} -> 
            case length(W)>0 of
                true ->
                    {RetPid,RetRef,Func,Args} = hd(W),
                    Pid ! {work,RetPid,RetRef,Func,Args},
                    pool_loop([],tl(W));
                false ->
                    pool_loop([Pid],W)
            end;
        {new_ws, Workers} -> pool_loop(Workers,W); %!!!What if there is work?!!!!
        {queue, Work} -> pool_loop([],[Work]++W);
        {exit, Pid} -> pool_loop([],[])
    end;
pool_loop([X | Xs],W) ->
    receive 
        {ask, Pid, Ref} -> 
            Pid ! {ok, Ref, X}, 
            pool_loop(Xs,W);
        {available, Pid} -> pool_loop([Pid] ++ [X|Xs],W);
        {queue, Work} -> 
            {RetPid, RetRef, Func, Args} = Work,
            X ! {work, RetPid, RetRef, Func, Args},
            pool_loop(Xs, W);
        {exit, Pid} -> pool_loop([X|Xs],[])
    end.


%% Waits for work. 
%% Runs the given work, sends it back and then makes itself available again.
worker(Pool_pid) ->
    receive
        {work, Pid, Ref, F, Args} ->  
                case catch apply(F, Args) of
                    {'EXIT', no_solution} ->
                                   Pid ! {error, self(), Ref},
                                   Pool_pid ! {available, self()},
                                   worker(Pool_pid);
                    Result -> Pid ! {result, self(), Ref, Result}, 
                              Pool_pid ! {available, self()}, 
                              worker(Pool_pid)
                end
    end.

%% Test workers 
test_pool(N) ->
    Pp = pool_start(N),
    Pp ! {ask, self()},
    receive 
        {ok, Wp} -> Wp ! {work, self(), fun() -> 1+1 end, []}, 
                    receive {result, _, R} -> 
                        io:fwrite("result: ~p~n\n", [R]) 
                    end
        %{no_avail, _} ->  
    end.
    

