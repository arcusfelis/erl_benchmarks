-module(jlib).
-compile(export_all).

-ifdef(TODO_BENCHMARK).
-include_lib("emark/include/emark.hrl").
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

make_jid(U,S,R) -> {S,U,R}.


%% binary_to_jid(J) when erlang:is_binary(J) ->
%%     binary_to_jid(binary_to_list(J));
old_binary_to_jid(S) -> old_binary_to_jid_(S).

old_binary_to_jid_(<<$/, _/binary>>) -> error;
old_binary_to_jid_(<<>>) -> error;
old_binary_to_jid_(J) ->
    binary_to_jid1(J, <<>>).

binary_to_jid1(<<$@, J/binary>>, N) ->
    binary_to_jid2(J, binary_reverse(N), <<>>);
binary_to_jid1(<<$/, J/binary>>, N) ->
    binary_to_jid3(J, <<>>, binary_reverse(N), <<>>);
binary_to_jid1(<<C, J/binary>>, N) ->
    binary_to_jid1(J, <<C, N/binary>>);
binary_to_jid1(<<>>, N) ->
    make_jid(<<>>, binary_reverse(N), <<>>).

%% Only one "@" is admitted per JID
binary_to_jid2(<<$@, _J/binary>>, _N, _S) ->
    error;
binary_to_jid2(<<$/, _J/binary>>, _N, <<>>) ->
    error;
binary_to_jid2(<<$/, J/binary>>, N, S) ->
    binary_to_jid3(J, N, binary_reverse(S), <<>>);
binary_to_jid2(<<C, J/binary>>, N, S) ->
    binary_to_jid2(J, N, <<C, S/binary>>);
binary_to_jid2(<<>>, _N, <<>>) ->
    error;
binary_to_jid2(<<>>, N, S) ->
    make_jid(N, binary_reverse(S), <<>>).

binary_to_jid3(<<C, J/binary>>, N, S, R) ->
    binary_to_jid3(J, N, S, <<C, R/binary>>);
binary_to_jid3(<<>>, N, S, R) ->
    make_jid(N, S, binary_reverse(R)).

binary_reverse(<<>>) ->
    <<>>;
binary_reverse(<<H,T/binary>>) ->
    <<(binary_reverse(T))/binary,H>>.


new_binary_to_jid(J) ->
    binary_to_to_jid1(J, <<>>).

binary_to_to_jid1(<<$@, _J/binary>>, <<>>) ->
    error;
binary_to_to_jid1(<<$@, J/binary>>, N) ->
    binary_to_to_jid2(J, reverse(N), <<>>);
binary_to_to_jid1(<<$/, _J/binary>>, <<>>) ->
    error;
binary_to_to_jid1(<<$/, J/binary>>, N) ->
    make_jid(<<>>, reverse(N), J);
binary_to_to_jid1(<<C, J/binary>>, N) ->
    binary_to_to_jid1(J, <<C, N/binary>>);
binary_to_to_jid1(<<>>, <<>>) ->
    error;
binary_to_to_jid1(<<>>, N) ->
    make_jid(<<>>, reverse(N), <<>>).

%% Only one "@" is admitted per JID
binary_to_to_jid2(<<$@, _J/binary>>, _N, _S) ->
    error;
binary_to_to_jid2(<<$/, _J/binary>>, _N, <<>>) ->
    error;
binary_to_to_jid2(<<$/, J/binary>>, N, S) ->
    make_jid(N, reverse(S), J);
binary_to_to_jid2(<<C, J/binary>>, N, S) ->
    binary_to_to_jid2(J, N, <<C, S/binary>>);
binary_to_to_jid2(<<>>, _N, <<>>) ->
    error;
binary_to_to_jid2(<<>>, N, S) ->
    make_jid(N, reverse(S), <<>>).

binary_to_to_jid3(R, N, S) ->
    {N,S,R}.


smart_binary_to_jid(J) ->
    case smart_binary_to_jid1(J, 0) of
        {_, _, 0} ->
            error;
        {_, <<>>, _} ->
            error;
        {$@, J2,  N} ->
            Name = binary:part(J, 0, N),
            case smart_binary_to_jid1(J2, 0) of
                {_, _, 0} ->
                    error;
                {_, <<>>, _} ->
                    error;
                {$/, Resource, N2} ->
                    {Name, binary:part(J2, 0, N2), Resource};
                {$@, _,  _} ->
                    error;
                nomatch ->
                    {Name, J2, <<>>}
            end;
        {$/, Resource, N} ->
            Server = binary:part(J, 0, N),
            {<<>>, Server, Resource};
        nomatch ->
            {<<>>, J, <<>>}
    end.

smart_binary_to_jid1(<<$@, J/binary>>, N) ->
    {$@, J, N};
smart_binary_to_jid1(<<$/, J/binary>>, N) ->
    {$/, J, N};
smart_binary_to_jid1(<<_, J/binary>>, N) ->
    smart_binary_to_jid1(J, N+1);
smart_binary_to_jid1(<<>>, _) ->
    nomatch.

split_binary1(B, N) when is_binary(B), is_integer(N) ->
    <<B1:N/binary, _, B2/binary>> = B,
    {B1, B2}.


-ifdef(TEST).
smart_binary_to_jid_test_() ->
    [{"name@server",
      ?_assertEqual({<<"romeo">>, <<"montague.net">>, <<>>},
                    smart_binary_to_jid(<<"romeo@montague.net">>))},
     {"server@resource",
      ?_assertEqual({<<>>, <<"montague.net">>, <<"home">>},
                    smart_binary_to_jid(<<"montague.net/home">>))},
     {"name@server/resource",
      ?_assertEqual({<<"romeo">>, <<"montague.net">>, <<"home">>},
                    smart_binary_to_jid(<<"romeo@montague.net/home">>))},
     {"server",
      ?_assertEqual({<<>>, <<"montague.net">>, <<>>},
                    smart_binary_to_jid(<<"montague.net">>))},
     {"errors", [
       ?_assertEqual(error, smart_binary_to_jid(<<"romeo@">>)),
       ?_assertEqual(error, smart_binary_to_jid(<<"romeo@montague@net">>)),
       ?_assertEqual(error, smart_binary_to_jid(<<"romeo@montague.net/">>))
      ]}
    ].
-endif.



reverse(Bin) when is_binary(Bin) ->
    S = bit_size(Bin),
    <<V:S/integer-little>> = Bin,
    <<V:S/integer-big>>.



new_binary_to_jid2(J) ->
    new_binary_to_jid2_(J).

new_binary_to_jid2_(J) ->
    case binary:matches(J, [<<"@">>, <<"/">>]) of
        [{D1,_}] ->
            case binary:at(J, D1) of
                $@ -> name_server(binary:part(J, 0, D1),
                                  binary:part(J, D1+1, byte_size(J) - D1 - 1));
                $/ -> server_resource(binary:part(J, 0, D1),
                                      binary:part(J, D1+1, byte_size(J) - D1 - 1))
            end;
        [{D1,_},{D2,_}|_] ->
            case {binary:at(J, D1), binary:at(J, D2)} of
                {$@, $/} ->
            name_server_resource(binary:part(J, 0, D1),
                                 binary:part(J, D1+1, D2 - D1 - 1),
                                 binary:part(J, D2+1, byte_size(J) - D2 - 1));
                _ -> error
            end;
        [] ->
            server(J)
    end.

new_binary_to_jid3(J) ->
    new_binary_to_jid3_(J).

new_binary_to_jid3_(<<>>) -> error;
new_binary_to_jid3_(J) when is_binary(J) ->
    case binary:matches(J, [<<"@">>, <<"/">>]) of
        []         -> server(J);
        [{0,_}|_]  -> error;
        [{D1,_}] ->
            {P1, P2} = erlang:split_binary(J, D1),
            case binary:at(J, D1) of
                $@ ->  name_server(P1, P2);
                $/ ->  server_resource(P1, P2)
            end;
        [{D1,_},{D2,_}|_] when D1 =:= D2+1 ->
            error;
        [{D1,_},{D2,_}|_] ->
            P1 = binary:part(J, 0, D1),
            P2 = binary:part(J, D1+1, D2 - D1 - 1),
            P3 = binary:part(J, D2+1, byte_size(J) - D2 - 1),
            name_server_resource(P1, P2, P3)
    end.

new_binary_to_jid4(J) ->
    new_binary_to_jid4_(J).

new_binary_to_jid4_(J) ->
    case binary:match(J, [<<"@">>, <<"/">>]) of
        {0,_}  -> error;
        {D1,_} ->
            case binary:at(J, D1) of
                $@ -> 
                    case binary:match(J, <<"/">>, [{scope,scope_from(J, D1+1)}]) of
                        {D2,_} ->
                            <<P1:D1/binary,_,P2:D2/binary,P3/binary>> = J,
                            name_server_resource(P1, P2, P3);
                        nomatch ->
                            {P1, P2} = erlang:split_binary(J, D1),
                            name_server(P1, P2)
                    end;
                $/ ->
                    {P1, P2} = erlang:split_binary(J, D1),
                    server_resource(P1, P2)
            end
    end.

scope_from(B, D) -> {D, byte_size(B)-D}.


new_binary_to_jid5(S) -> new_binary_to_jid5_(S).

new_binary_to_jid5_(<<>>) -> error;
new_binary_to_jid5_(B) when is_binary(B) ->
    case binary:matches(B, [<<"@">>, <<"/">>]) of
        []        -> server(B);
        [{0,_}|_] -> error;
        [{P1,_}]  ->
            case B of
                <<N:P1/binary, "@", S/binary>> -> make_jid(N, S, <<>>);
                <<S:P1/binary, "/", R/binary>> -> make_jid(<<>>, S, R)
            end;
        [{P1,_},{P2,_}|_] ->
            L2 = P2 - P1 - 1,
            case B of
                <<N:P1/binary, "@", S:L2/binary, "/", R/binary>> when L2 > 0 ->
                    make_jid(N, S, R);
                _ -> error
            end
    end.
            

server(S) ->
    make_jid(<<>>, S, <<>>).

name_server(N, S) ->
    make_jid(N, S, <<>>).

name_server_resource(N, S, R) ->
    make_jid(N, S, R).

server_resource(S, R) ->
    make_jid(<<>>, S, R).

-ifdef(BENCHMARK).

old_binary_to_jid_benchmark(N) ->
    Xs = gen_binaries(N, 250),
    emark:start(?MODULE, old_binary_to_jid, 1),
    [?MODULE:old_binary_to_jid(X) || X <- Xs],
    ok.

new_binary_to_jid_benchmark(N) ->
    Xs = gen_binaries(N, 250),
    emark:start(?MODULE, new_binary_to_jid, 1),
    [?MODULE:new_binary_to_jid(X) || X <- Xs],
    ok.

new_binary_to_jid2_benchmark(N) ->
    Xs = gen_binaries(N, 250),
    emark:start(?MODULE, new_binary_to_jid2, 1),
    [?MODULE:new_binary_to_jid2(X) || X <- Xs],
    ok.

new_binary_to_jid3_benchmark(N) ->
    Xs = gen_binaries(N, 250),
    emark:start(?MODULE, new_binary_to_jid3, 1),
    [?MODULE:new_binary_to_jid3(X) || X <- Xs],
    ok.

new_binary_to_jid4_benchmark(N) ->
    Xs = gen_binaries(N, 250),
    emark:start(?MODULE, new_binary_to_jid4, 1),
    [?MODULE:new_binary_to_jid4(X) || X <- Xs],
    ok.

new_binary_to_jid5_benchmark(N) ->
    Xs = gen_binaries(N, 250),
    emark:start(?MODULE, new_binary_to_jid5, 1),
    [?MODULE:new_binary_to_jid5(X) || X <- Xs],
    ok.


smart_binary_to_jid_benchmark(N) ->
    Xs = gen_binaries(N, 250),
    emark:start(?MODULE, smart_binary_to_jid, 1),
    [?MODULE:smart_binary_to_jid(X) || X <- Xs],
    ok.


gen_binaries(0, L) -> [];
gen_binaries(N, L) when N > 0 ->
    Rand = crypto:rand_bytes(L div 3),
    [ <<Rand/binary, "@", Rand/binary, "/", Rand/binary>> | gen_binaries(N-1, L)].

-endif.
