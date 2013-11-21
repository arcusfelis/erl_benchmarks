-module(repeat).
-compile(export_all).

repeat(B, C) when C > 0 ->
    repeat(B, C, <<>>).

repeat(_, 0, R) ->
    R;
repeat(B, C, R) ->
    repeat(B, C-1, <<R/binary, B/binary>>).


repeat2(B, C) when C > 0 ->
    repeat2(B, C, <<>>).

repeat2(_, 0, R) ->
    R;
repeat2(B, C, R) ->
    repeat2(B, C-1, <<B/binary, R/binary>>).

repeat3(B, C) when C > 0 ->
    << <<X/binary>> || X <- lists:duplicate(C,B) >>.

repeat4(B, C) when C > 0 ->
    iolist_to_binary(lists:duplicate(C,B)).

-include_lib("eunit/include/eunit.hrl").


-ifdef(BENCHMARK).
-include_lib("emark/include/emark.hrl").

%% suffix
repeat_benchmark(N) ->
    Xs = [random:uniform(20) || _ <- lists:seq(1, N)],
    emark:start(?MODULE, repeat, 2),
    [?MODULE:repeat(<<1,2,3>>, X) || X <- Xs],
    ok.

%% prefix
repeat2_benchmark(N) ->
    Xs = [random:uniform(20) || _ <- lists:seq(1, N)],
    emark:start(?MODULE, repeat2, 2),
    [?MODULE:repeat2(<<1,2,3>>, X) || X <- Xs],
    ok.

%% LC
repeat3_benchmark(N) ->
    Xs = [random:uniform(20) || _ <- lists:seq(1, N)],
    emark:start(?MODULE, repeat3, 2),
    [?MODULE:repeat3(<<1,2,3>>, X) || X <- Xs],
    ok.

%% iolists
repeat4_benchmark(N) ->
    Xs = [random:uniform(20) || _ <- lists:seq(1, N)],
    emark:start(?MODULE, repeat4, 2),
    [?MODULE:repeat4(<<1,2,3>>, X) || X <- Xs],
    ok.

-endif.
