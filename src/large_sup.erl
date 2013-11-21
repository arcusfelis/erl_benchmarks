%% @author Magnus Klaar <magnus.klaar@sgsstudentbostader.se>
%% @doc Maintain a pool of io_file processes
%% <p>This very simple supervisor keeps track of a set of file
%% processes for the I/O subsystem.</p>
%% @end
-module(large_sup).
-behaviour(supervisor).

%% Use a separate supervisor for files. This ensures that
%% the directory server can assume that all files will be
%% closed if it crashes.

-export([start_link/1]).
-export([init/1]).


%% @doc Start the file pool supervisor
%% @end
start_link(X) ->
    supervisor:start_link(?MODULE, [X]).

%% @private
init([X]) ->
    FileSpecs  = [file_server_spec(N) || N <- lists:seq(0, X)],
    {ok, {{one_for_all, 1, 60}, FileSpecs}}.

file_server_spec(N) ->
    {N,
        {large_worker, start_link, []},
        permanent, 2000, worker, [large_worker]}.
