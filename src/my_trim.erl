-module(my_trim).
-compile(export_all).


trim(<<0, Bin/binary>>) ->
    trim(Bin);
trim(X) ->
    X.


trim2(B) ->
    S = byte_size(B),
    do_trim(0, B, S).

do_trim(X, _B, X) ->
    <<>>;
do_trim(S, B, X) ->
    case binary:at(B, S) of
        0 -> do_trim(S+1, B, X);
        _ -> binary_part(B, S, X-S)
    end.


trim3(B) ->
    C = do_trim3(B, 0),
    binary:part(B, C, byte_size(B) - C).


re_trim(B) ->
    re:replace(B, "^\\s+|\\s+$", "", [global, {return, binary}]).


re_compile() ->
    {ok, RE} = re:compile("^\\s+|\\s+$"),
    RE.


re_compiled_trim(RE, B) ->
    re:replace(B, RE, "", [global, {return, binary}]).


do_trim3(<<0, Bin/binary>>, C) ->
    do_trim3(Bin, C+1);
do_trim3(_B, C) ->
    C.

-include_lib("eunit/include/eunit.hrl").


-ifdef(BENCHMARK).
-include_lib("emark/include/emark.hrl").

trim_benchmark(N) ->
    Xs = gen_binaries(N),
    emark:start(?MODULE, trim, 1),
    [?MODULE:trim(X) || X <- Xs],
    ok.

trim2_benchmark(N) ->
    Xs = gen_binaries(N),
    emark:start(?MODULE, trim2, 1),
    [R = ?MODULE:trim2(X) || {X, R} <- Xs],
    ok.

trim3_benchmark(N) ->
    Xs = gen_binaries(N),
    emark:start(?MODULE, trim3, 1),
    [R = ?MODULE:trim3(X) || {X, R} <- Xs],
    ok.

re_trim_benchmark(N) ->
    Xs = gen_binaries(N),
    emark:start(?MODULE, re_trim, 1),
    [?MODULE:re_trim(X) || X <- Xs],
    ok.

re_compiled_trim_benchmark(N) ->
    Xs = gen_binaries(N),
    RE = re_compile(),
    emark:start(?MODULE, re_compiled_trim, 2),
    [?MODULE:re_compiled_trim(RE, X) || X <- Xs],
    ok.

gen_binaries(0) -> [];
gen_binaries(N) when N > 0 ->
    X = random:uniform(50) - 1,
    Bits = X * 8,
    [ {<<0:Bits, 1:Bits, 0:Bits>>, <<1:Bits>>} | gen_binaries(N-1)].


gen_binaries2(0) -> [];
gen_binaries2(N) when N > 0 ->
    X = random:uniform(50) - 1,
    B = << <<$ >> || _ <- lists:seq(1, X) >>,
    Bits = X * 8,
    [ <<B/binary, 0:Bits, B/binary>> | gen_binaries2(N-1)].

-endif.
