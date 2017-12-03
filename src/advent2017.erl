%%%-------------------------------------------------------------------
%% @doc advent2017 public API
%% @end
%%%-------------------------------------------------------------------

-module(advent2017).

-behaviour(application).
% Application behaviour
-export([start/2,stop/1]).
% Own functions
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
%% Internal functions
%%====================================================================

challenge(Challenge) ->
    case Challenge of
        0 -> challenge_zero:run()
    end.

challengeAll() ->
    [challenge(C) || C <- [0]].
