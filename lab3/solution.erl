-module(solution).
-compile(export_all).

%% returns the pid of the pool manager
pool_start(nbr_workers) ->
    Pool_pid <- spawn_link(pool_init()),
    Ws <- start_workers(nbr_workers, Pool_pid),
    Pool_pid ! {w, WS},
    Pool_pid.

pool_init() ->
    receive 
        {w, Workers} -> pool_loop(Workers).

start_workers(0, Pp) -> [spawn_link(worker(Pp))].
start_workers(X, Pp) -> [spawn_link(worker(Pp)) | start_workers(X-1, Pp)].

%% Waits for messages from workgivers or workers finished with some work.
%% Calls itself
pool_loop([]) ->
    receive 
        {ask, Pid} -> Pid ! {no_avail, 0}, pool_loop([]).
        {available, Pid} -> pool_loop([Pid]).
        {exit, Pid} -> . %% handle shutdown of workers.
pool_loop([X | Xs]) ->
    receive 
        {ask, Pid} -> Pid ! {ok, X}, 
                      pool_loop(Xs).
        {available, Pid} -> pool_loop([Pid] ++ [X|Xs]).
        {exit, Pid} -> . %% handle shutdown off workers.


%% Waits for work. 
%% Runs the given work, sends it back and then makes itself available again.
worker(Pool_pid) ->
    receive
        {work, Pid, F, Args} -> Pid ! {result, F(Args)}, 
                                Pool_pid ! {available, self()}, 
                                worker(Pool_pid).

%% Test workers 
test_pool() ->
    Pp <- pool_start(2),
    Pp ! {ask, self()},
    receive 
        {ok, Wp} -> Wp ! {work, self(), fun(x) -> x+1 end, [2]},
        {no_avail, _} -> .
    receive 
        {result, r} -> r.
    

%% Tries to parallize the solving by running the refinements of rows in parallel.
par_refine(M) ->


%% Tries to parallize the solving by running the guesses in parallel.
par_guesses(M) ->
