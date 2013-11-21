-module(join).
-compile(export_all).


join([B|Bs], Sep) when is_binary(Sep) ->
    R = << <<Sep/binary, X/binary>> || X <- Bs >>,
    <<B/binary, R/binary>>;

join([], _Sep) ->
    <<>>.


join2([B|Bs], Sep) when is_binary(Sep) ->
    R = [ [Sep, X] || X <- Bs ],
    iolist_to_binary([B, R]);

join2([], _Sep) ->
    <<>>.



join3([B|Bs], Sep) when is_binary(Sep) ->
    iolist_to_binary([B|add_separator(Bs, Sep)]);

join3([], _Sep) ->
    <<>>.

add_separator([H|T], S) ->
    [S, H | add_separator(T, S)];
add_separator([], _) ->
    [].


-include_lib("eunit/include/eunit.hrl").


-ifdef(BENCHMARK).
-include_lib("emark/include/emark.hrl").

join_benchmark(N) ->
    Xs = gen_binaries(N, 100, 10, 10),
    emark:start(?MODULE, join, 2),
    [?MODULE:join(X, <<0>>) || X <- Xs],
    ok.

join2_benchmark(N) ->
    Xs = gen_binaries(N, 100, 10, 10),
    emark:start(?MODULE, join2, 2),
    [?MODULE:join2(X, <<0>>) || X <- Xs],
    ok.

join3_benchmark(N) ->
    Xs = gen_binaries(N, 100, 10, 10),
    emark:start(?MODULE, join3, 2),
    [?MODULE:join3(X, <<0>>) || X <- Xs],
    ok.

gen_binaries(0, K, T, L) -> [];
gen_binaries(N, K, T, L) when N > 0 ->
    Rand = gen_binaries2(K, T),
    [ Rand | gen_binaries(N-1, K, T, L)].

gen_binaries2(0, L) -> [];
gen_binaries2(N, L) when N > 0 ->
    Rand = crypto:rand_bytes(L),
    [ Rand | gen_binaries2(N-1, L)].

-endif.
