-module(unique).
-compile(export_all).

-ifdef(BENCHMARK).
-include_lib("emark/include/emark.hrl").
-endif.

sort_unique(L) ->
    keys(2, lists:keysort(1, lists:ukeysort(2, enumerate(L)))).

keys(N, List) ->
    [element(N, X) || X <- List].

enumerate(L) ->
    enumerate(L, 1).

enumerate([H|T], N) ->
    [{N, H} | enumerate(T, N+1)];
enumerate([], _N) ->
    [].


rec_unique([H|T]) ->
    [H|rec_unique(delete_all(H, T))];
rec_unique([]) ->
    [].

delete_all(X, [X|T]) -> delete_all(X, T);
delete_all(X, [H|T]) -> [H|delete_all(X, T)];
delete_all(_, []) -> [].


-ifdef(BENCHMARK).

sort_unique_benchmark(N) ->
    Xs = gen_list2(N, 20),
    emark:start(?MODULE, sort_unique, 1),
    [?MODULE:sort_unique(X) || X <- Xs],
    ok.

rec_unique_benchmark(N) ->
    Xs = gen_list2(N, 20),
    emark:start(?MODULE, rec_unique, 1),
    [?MODULE:rec_unique(X) || X <- Xs],
    ok.


gen_list2(M, L) ->
    [gen_list(L) || _ <- lists:seq(1, M)].

gen_list(L) ->
    binary_to_list(crypto:rand_bytes(L)).

-endif.
