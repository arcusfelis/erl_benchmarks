-module(group_count_with).
-compile(export_all).



group_count_with1(_keymaker, []) ->
    [];

group_count_with1(KeyMaker, List) ->
    %% Map
    Mapped = [KeyMaker(X) || X <- List],
    SortedMapped = lists:sort(Mapped),
    count_sorted_clusters(SortedMapped).

count_sorted_clusters([H|T]) ->
    count_sorted_clusters(T, H, 1);
count_sorted_clusters([]) ->
    [].

count_sorted_clusters([H|T], H, N) ->
    count_sorted_clusters(T, H, N+1);
count_sorted_clusters([H|T], K, N) ->
    [{K,N}|count_sorted_clusters(T, H, 1)];
count_sorted_clusters([], H, N) ->
    [{H,N}].


count_groups(KM, List) ->
    count_groups(KM, List, dict:new());
count_groups(KM, []) ->
    [].

count_groups(KM, [H|T], A) ->
    A2 = dict:update_counter(KM(H), 1, A),
    count_groups(KM, T, A2);
count_groups(_KM, [], A) ->
    dict:to_list(A).




-include_lib("eunit/include/eunit.hrl").


-ifdef(BENCHMARK).
-include_lib("emark/include/emark.hrl").

-define(N, 1000).

group_count_with1_benchmark(N) ->
    Xs = [[random:uniform(10) || _ <- lists:seq(1, ?N)] || _ <- lists:seq(1, N)],
    emark:start(?MODULE, group_count_with1, 2),
    [?MODULE:group_count_with1(fun(Y) -> Y end, X) || X <- Xs],
    ok.

count_groups_benchmark(N) ->
    Xs = [[random:uniform(10) || _ <- lists:seq(1, ?N)] || _ <- lists:seq(1, N)],
    emark:start(?MODULE, count_groups, 2),
    [?MODULE:count_groups(fun(Y) -> Y end, X) || X <- Xs],
    ok.

-endif.
