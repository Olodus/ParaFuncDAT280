-module(solution)
-compile(export_all)

%% returns the pid of the pool manager
pool_start(nbr_workers) ->
    Ws <- start_workers(nbr_workers),
    pool_loop(Ws).

%% Waits for messages from workgivers or workers finished with some work.
%% Calls itself
pool_loop(Xs) ->
    receive 
        {ask, Pid} -> 
        {exit, Pid} ->
        {available, Pid} ->

start_workers(0) -> [spawn_link(worker())].
start_workers(X) -> [spawn_link(worker()) | start_workers(X-1)].

%% Waits for work. 
%% Runs the given work, sends it back and then makes itself available again.
worker() ->
    receive
        {work, Pid, F, Args} -> 



%% Tries to parallize the solving by running the refinements of rows in parallel.
par_refine(M) ->


%% Tries to parallize the solving by running the guesses in parallel.
par_guesses(M) ->
