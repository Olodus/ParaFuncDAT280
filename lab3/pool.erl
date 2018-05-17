-module(pool).
-compile(export_all).

%% returns the pid of the pool manager
pool_start(Nbr_workers) ->
    Pool_pid = spawn_link(pool,pool_loop,[[],[]]),
    Ws = start_workers(Nbr_workers, Pool_pid),
    Pool_pid ! {new_ws, Ws},
    Pool_pid.

start_workers(0, Pp) -> [spawn_link(pool, worker, [Pp])];
start_workers(X, Pp) -> [spawn_link(pool, worker, [Pp]) | start_workers(X-1, Pp)].

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
        %{new_ws, Workers} -> pool_loop(Workers ++ [X|Xs]) we probably shouldn't allow this
        {exit, Pid} -> pool_loop([X|Xs],[])
    end.


%% Waits for work. 
%% Runs the given work, sends it back and then makes itself available again.
worker(Pool_pid) ->
    receive
        {work, Pid, Ref, F, Args} ->  
                case catch apply(F, Args) of
                    {'EXIT', no_solution} -> %io:fwrite("Exception thrown in worker ~n"),
                                    %io:fwrite("Args are: ~w ~n",[Args]),
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
    

