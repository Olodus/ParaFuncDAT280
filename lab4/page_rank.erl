%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This implements a page rank algorithm using map-reduce
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(page_rank).
-import(pool,[pool_start/1]).
-compile(export_all).

%% Use map_reduce to count word occurrences

map(Url,ok) ->
    [{Url,Body}] = dets:lookup(web,Url),
    Urls = crawl:find_urls(Url,Body),
    [{U,1} || U <- Urls].

reduce(Url,Ns) ->
    [{Url,lists:sum(Ns)}].

page_rank() ->
    {ok,web} = dets:open_file(web,[{file,"web.dat"}]),
    Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
    map_reduce:map_reduce_seq(fun map/2, fun reduce/2, 
			      [{Url,ok} || Url <- Urls]).

page_rank_par() ->
    dets:open_file(web,[{file,"web.dat"}]),
    Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
    map_reduce:map_reduce_par(fun page_rank:map/2, 32, fun page_rank:reduce/2, 32, 
			      [{Url,ok} || Url <- Urls]).


page_rank_dis() ->
    dets:open_file(web,[{file,"web.dat"}]),
    Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
    map_reduce:map_reduce_dis(fun page_rank:map/2, 32, fun page_rank:reduce/2, 32, 
			      [{Url,ok} || Url <- Urls]).



page_rank_dis_bal() ->
    dets:open_file(web,[{file,"web.dat"}]),
    PoolPids = pool:pool_start(4),
    Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
    map_reduce:map_reduce_dis_bal(PoolPids, fun page_rank:map/2, 32, fun page_rank:reduce/2, 32, 
			      [{Url,ok} || Url <- Urls]).

page_rank_dis_bal_new() ->
    dets:open_file(web,[{file,"web.dat"}]),
    PoolPids = pool:pool_start(4),
    Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
    map_reduce:map_reduce_dis_bal_new(PoolPids, fun page_rank:map/2, 32, fun page_rank:reduce/2, 32, 
			      [{Url,ok} || Url <- Urls]).
