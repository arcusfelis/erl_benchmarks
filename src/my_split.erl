-module(my_split).
-compile(export_all).

split_binary_with(<<>>, _Delim) ->
    [];

split_binary_with(Bin, Delim) ->
    DelimPos = first(Bin, Delim),
    case DelimPos of
    undefined ->
        [Bin];
    _ ->
        <<H:DelimPos/binary, _, T/binary>> = Bin,
        [H | split_binary_with(T, Delim)]
    end.


first(<<0, _/binary>>, Idx) ->
    Idx;
first(<<_, T/binary>>, Idx) ->
    first(T, Idx+1);
first(<<>>, _Idx) ->
    undefined.



my_split(Bin, Delim) ->
    split_binary_with(Bin, Delim).


standard_split(Bin, Delim) ->
    binary:split(Bin, <<Delim>>, [global]).


standard_split2(Bin, Pattern) ->
    binary:split(Bin, Pattern, [global]).


-include_lib("eunit/include/eunit.hrl").

split_string_test_() ->
    [?_assertEqual(split_binary_with(<<1,2,3,0,4,5,6>>, 0), 
                   [<<1,2,3>>, <<4,5,6>>])
    ,?_assertEqual(split_binary_with(<<1,2,3,0,4,5,6,0>>, 0), 
                   [<<1,2,3>>, <<4,5,6>>])].



-ifdef(TODO_BENCHMARK).
-include_lib("emark/include/emark.hrl").

standard_split_benchmark(N) ->
    Xs = gen_binaries(N, 1000),
    emark:start(?MODULE, standard_split, 2),
    [?MODULE:standard_split(X, 0) || X <- Xs],
    ok.


standard_split_with_compiled_pattern_benchmark(N) ->
    Xs = gen_binaries(N, 1000),
    P = binary:compile_pattern(<<0>>),
    emark:start(?MODULE, standard_split2, 2),
    [?MODULE:standard_split2(X, P) || X <- Xs],
    ok.


my_split_benchmark(N) ->
    Xs = gen_binaries(N, 1000),
    emark:start(?MODULE, my_split, 2),
    [?MODULE:my_split(X, 0) || X <- Xs],
    ok.


gen_binaries(0, L) -> [];
gen_binaries(N, L) when N > 0 ->
    Rand = crypto:rand_bytes(L),
    [ <<Rand/binary, 0>> | gen_binaries(N-1, L)].

-endif.
