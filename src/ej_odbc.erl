-module(ej_odbc).
-compile(export_all).

-ifdef(TODO_BENCHMARK).
-include_lib("emark/include/emark.hrl").
-endif.

%% Escape character that will confuse an SQL engine
escape(S) when is_binary(S) ->
    list_to_binary(escape(binary_to_list(S)));
escape(S) when is_list(S) ->
    S1 = lists:foldl(fun(C, Acc) -> [escape_char(C) | Acc] end,
                     [], S),
    lists:reverse(S1).

%% Characters to escape
escape_char($\0) -> "\\0";
escape_char($\n) -> "\\n";
escape_char($\t) -> "\\t";
escape_char($\b) -> "\\b";
escape_char($\r) -> "\\r";
escape_char($')  -> "\\'";
escape_char($")  -> "\\\"";
escape_char($\\) -> "\\\\";
escape_char(C)   -> C.

escape_char1($\0) -> "\\0";
escape_char1($\n) -> "\\n";
escape_char1($\t) -> "\\t";
escape_char1($\b) -> "\\b";
escape_char1($\r) -> "\\r";
escape_char1($')  -> "\\'";
escape_char1($")  -> "\\\"";
escape_char1($\\) -> "\\\\";
escape_char1(C)   -> [C].

escape_char_bin($\0) -> <<"\\0">>;
escape_char_bin($\n) -> <<"\\n">>;
escape_char_bin($\t) -> <<"\\t">>;
escape_char_bin($\b) -> <<"\\b">>;
escape_char_bin($\r) -> <<"\\r">>;
escape_char_bin($')  -> <<"\\'">>;
escape_char_bin($")  -> <<"\\\"">>;
escape_char_bin($\\) -> <<"\\\\">>;
escape_char_bin(C)   -> <<C>>.

old_escape(S) ->
    escape(S).

new_escape(S) ->
    escape2(S).

new_escape_replace(S) ->
    escape3(S).

new_escape_simple(S) ->
    escape4(S).

new_escape_badidea(S) ->
    escape5(S).

new_escape_stupid(S) ->
    escape6(S).

escape2(S) when is_binary(S) ->
    << <<(escape_char_bin(C))/binary>> || <<C>> <= S>>;
escape2(S) when is_list(S) ->
    [escape_char(C) || C <- S].


escape3(S) when is_binary(S) ->
    EscChars = [<<"\0">>, <<"\n">>, <<"\t">>, <<"\b">>, <<"\r">>,
                <<$'>>, <<$">>, <<$\\>>],
    binary:replace(S, EscChars, <<"\\">>, [global, {insert_replaced, 1}]);
escape3(S) when is_list(S) ->
    [escape_char(C) || C <- S].


escape4(S) when is_binary(S) ->
    iolist_to_binary(escape4(binary_to_list(S)));
escape4(S) when is_list(S) ->
    [escape_char(C) || C <- S].


escape5(S) when is_binary(S) ->
    iolist_to_binary([escape_char(C) || <<C>> <= S]);
escape5(S) when is_list(S) ->
    [escape_char(C) || C <- S].


escape6(S) when is_binary(S) ->
    << <<CC>> || <<C>> <= S, CC <- escape_char1(C) >>;
escape6(S) when is_list(S) ->
    [escape_char(C) || C <- S].



-ifdef(BENCHMARK).

old_escape_benchmark(N) ->
    Xs = gen_binaries(N, 25),
    emark:start(?MODULE, old_escape, 1),
    [?MODULE:old_escape(X) || X <- Xs],
    ok.

new_escape_benchmark(N) ->
    Xs = gen_binaries(N, 25),
    emark:start(?MODULE, new_escape, 1),
    [?MODULE:new_escape(X) || X <- Xs],
    ok.

new_escape_replace_benchmark(N) ->
    Xs = gen_binaries(N, 25),
    emark:start(?MODULE, new_escape_replace, 1),
    [?MODULE:new_escape_replace(X) || X <- Xs],
    ok.

new_escape_simple_benchmark(N) ->
    Xs = gen_binaries(N, 25),
    emark:start(?MODULE, new_escape_simple, 1),
    [?MODULE:new_escape_simple(X) || X <- Xs],
    ok.

new_escape_badidea_benchmark(N) ->
    Xs = gen_binaries(N, 25),
    emark:start(?MODULE, new_escape_badidea, 1),
    [?MODULE:new_escape_badidea(X) || X <- Xs],
    ok.

new_escape_stupid_benchmark(N) ->
    Xs = gen_binaries(N, 25),
    emark:start(?MODULE, new_escape_stupid, 1),
    [?MODULE:new_escape_stupid(X) || X <- Xs],
    ok.


gen_binaries(0, L) -> [];
gen_binaries(N, L) when N > 0 ->
    case random:uniform(2) of
        1 -> [ crypto:rand_bytes(L) | gen_binaries(N-1, L)];
        2 -> [ binary_to_list(crypto:rand_bytes(L)) | gen_binaries(N-1, L)]
    end.

-endif.
