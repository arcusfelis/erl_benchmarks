-module(decode_string).
-compile(export_all).

-ifdef(TODO_BENCHMARK).
-include_lib("emark/include/emark.hrl").
-endif.

%% decode a single null-terminated string
new_decode_string(Bin) ->
    Idx = first(Bin, 0),
    <<H:Idx/binary, _, T/binary>> = Bin,
    {H, T}.

new_decode_string2(Bin) ->
    Idx = first2(Bin, 0),
    <<H:Idx/binary, _, T/binary>> = Bin,
    {H, T}.

new_decode_string3(Bin) ->
    Idx = first2(Bin, 0),
    {H, <<_, T/binary>>} = split_binary(Bin, Idx),
    {H, T}.

std_decode_string(Bin) ->
    [H, T] = binary:split(Bin, <<0>>),
    {H, T}.


first(<<X, T/binary>>, Idx) ->
    case X of
    0 -> Idx;
    _ -> first(T, Idx+1)
    end.

first2(<<0, _/binary>>, Idx) ->
    Idx;
first2(<<_, T/binary>>, Idx) ->
    first2(T, Idx+1).


old_decode_string(Bin) ->
    decode_string(Bin, <<>>).

decode_string(<<0, Rest/binary>>, Str) ->
    {Str, Rest};
decode_string(<<C, Rest/binary>>, Str) ->
    decode_string(Rest, <<Str/binary, C>>).



-ifdef(BENCHMARK).

old_decode_string_benchmark(N) ->
    Xs = gen_binaries(N, 1000),
    emark:start(?MODULE, old_decode_string, 1),
    [?MODULE:old_decode_string(X) || X <- Xs],
    ok.

new_decode_string_benchmark(N) ->
    Xs = gen_binaries(N, 1000),
    emark:start(?MODULE, new_decode_string, 1),
    [?MODULE:new_decode_string(X) || X <- Xs],
    ok.

new_decode_string2_benchmark(N) ->
    Xs = gen_binaries(N, 1000),
    emark:start(?MODULE, new_decode_string2, 1),
    [?MODULE:new_decode_string2(X) || X <- Xs],
    ok.

new_decode_string3_benchmark(N) ->
    Xs = gen_binaries(N, 1000),
    emark:start(?MODULE, new_decode_string3, 1),
    [?MODULE:new_decode_string3(X) || X <- Xs],
    ok.

std_benchmark(N) ->
    Xs = gen_binaries(N, 1000),
    emark:start(?MODULE, std_decode_string, 1),
    [?MODULE:std_decode_string(X) || X <- Xs],
    ok.



gen_binaries(0, L) -> [];
gen_binaries(N, L) when N > 0 ->
    Rand = crypto:rand_bytes(L),
    [ <<Rand/binary, 0>> | gen_binaries(N-1, L)].
    

-endif.
