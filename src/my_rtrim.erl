-module(my_rtrim).
-compile(export_all).



trim(Bin) ->
    L1 = binary_to_list(Bin),
    L2 = lists:reverse(L1),
    L3 = do_ltrim(L2),
    L4 = lists:reverse(L3),
    list_to_binary(L4).

do_ltrim([0|T]) -> 
    do_ltrim(T);
do_ltrim(X) -> 
    X.

trim2(B) ->
    S = byte_size(B),
    do_trim(S, B, 0).

do_trim(0, _B, _X) ->
    <<>>;
do_trim(S, B, X) ->
    S2 = S - 1,
    case binary:at(B, S2) of
        X -> do_trim(S2, B, X);
        _ -> binary_part(B, 0, S)
    end.

trim3(B) ->
    S = byte_size(B),
    do_trim0(S, B).

do_trim0(0, _B) ->
    <<>>;
do_trim0(S, B) ->
    S2 = S - 1,
    case binary:at(B, S2) of
        0 -> do_trim0(S2, B);
        _ -> binary_part(B, 0, S)
    end.

-include_lib("eunit/include/eunit.hrl").


-ifdef(TODO_BENCHMARK).
-include_lib("emark/include/emark.hrl").

trim_benchmark(N) ->
    Xs = gen_binaries(N),
    emark:start(?MODULE, trim, 1),
    [?MODULE:trim(X) || X <- Xs],
    ok.

trim3_benchmark(N) ->
    Xs = gen_binaries(N),
    emark:start(?MODULE, trim3, 1),
    [?MODULE:trim3(X) || X <- Xs],
    ok.

trim2_benchmark(N) ->
    Xs = gen_binaries(N),
    emark:start(?MODULE, trim2, 1),
    [?MODULE:trim2(X) || X <- Xs],
    ok.

gen_binaries(0) -> [];
gen_binaries(N) when N > 0 ->
    L = random:uniform(100),
    Rand = crypto:rand_bytes(L),
    X = random:uniform(5) - 1,
    Bits = X * 8,
    [ <<Rand/binary, 0:Bits>> | gen_binaries(N-1)].

-endif.
