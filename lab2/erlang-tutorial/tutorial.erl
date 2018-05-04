-module(tutorial).
-export([seq_msort/1]).



% Helper used to generate test data
random_list(N) -> [random:uniform(1000000) || _ <- lists:seq(1,N)].


merge([], Y) -> Y;
merge(X, []) -> X;
merge([X|Xs], [Y|Ys]) -> 
    if 
        X < Y -> [X|merge(Xs, [Y|Ys])];
        true -> [Y|merge(Ys, [X|Xs])]
    end.


seq_msort([]) -> [];
seq_msort(X) -> 
    Len = length(X),
    if 
        Len > 1 ->
            {L1,L2} = lists:split((round(Len/2)), X),
            merge(seq_msort(L1), seq_msort(L2));
        true -> 
            X
    end.






