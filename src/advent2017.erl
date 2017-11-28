%%%-------------------------------------------------------------------
%% @doc advent2017 public API
%% @end
%%%-------------------------------------------------------------------

-module(advent2017).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% The interface for outside
-export([challenge/1,challengeAll/0]).

%%====================================================================
%% API
%%====================================================================
-spec start(application:start_type(), term()) ->
    {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
    advent2017_sup:start_link().

%%--------------------------------------------------------------------
-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%====================================================================
%% Interface functions
%%====================================================================

challenge(Challenge) ->
    advent2017_server:run_challenge(Challenge).

challengeAll() ->
    [challenge(C) || C <- [0]].
