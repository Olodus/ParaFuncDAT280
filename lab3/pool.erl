-module(pool).
-compile(export_all).

%% returns the pid of the pool manager
pool_start(Nbr_workers) ->
    Pool_pid = spawn_link(pool,pool_loop,[[]]),
    Ws = start_workers(Nbr_workers, Pool_pid),
    Pool_pid ! {new_ws, Ws},
    Pool_pid.

start_workers(0, Pp) -> [spawn_link(pool, worker, [Pp])];
start_workers(X, Pp) -> [spawn_link(pool, worker, [Pp]) | start_workers(X-1, Pp)].

%% Waits for messages from workgivers or workers finished with some work.
%% Calls itself
pool_loop([]) ->
    receive 
        {ask, Pid, Ref} -> Pid ! {no_avail, Ref, 0}, pool_loop([]);
        {available, Pid} -> pool_loop([Pid]);
        {new_ws, Workers} -> pool_loop(Workers)
        %{exit, Pid} ->  %% handle shutdown of workers.
    end;
pool_loop([X | Xs]) ->
    receive 
        {ask, Pid, Ref} -> Pid ! {ok, Ref, X}, 
                      pool_loop(Xs);
        {available, Pid} -> pool_loop([Pid] ++ [X|Xs]);
        {new_ws, Workers} -> pool_loop(Workers ++ [X|Xs])
        %{exit, Pid} ->. %% handle shutdown off workers.
    end.


%% Waits for work. 
%% Runs the given work, sends it back and then makes itself available again.
worker(Pool_pid) ->
    receive
        {work, Pid, Ref, F, Args} ->  
                case catch apply(F, Args) of
                    {'EXIT', no_solution} -> io:fwrite("Exception thrown in worker ~n"),
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
    

