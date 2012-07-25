-module(decode_string).
-compile(export_all).

-ifdef(BENCHMARK).
-include_lib("emark/include/emark.hrl").
-endif.

%% decode a single null-terminated string
new_decode_string(Bin) ->
    Idx = first(Bin, 0),
    <<H:Idx/binary, _, T/binary>> = Bin,
    {H, T}.

first(<<X, T/binary>>, Idx) ->
    case X of
    0 -> Idx;
    _ -> first(T, Idx+1)
    end.


old_decode_string(Bin) ->
    decode_string(Bin, <<>>).

decode_string(<<0, Rest/binary>>, Str) ->
    {Str, Rest};
decode_string(<<C, Rest/binary>>, Str) ->
    decode_string(Rest, <<Str/binary, C>>).



-ifdef(BENCHMARK).

old_decode_string_benchmark(N) ->
    Xs = gen_binaries(1000, 1000),
    emark:start(?MODULE, old_decode_string, 2),
    [?MODULE:old_decode_string(X) || X <- Xs],
    ok.

new_decode_string_benchmark(N) ->
    Xs = gen_binaries(1000, 1000),
    emark:start(?MODULE, old_decode_string, 2),
    [?MODULE:new_decode_string(X) || X <- Xs],
    ok.



gen_binaries(0, L) -> [];
gen_binaries(N, L) when N > 0 ->
    [crypto:rand_bytes(L) | gen_binaries(N-1, L)].
    

-endif.
