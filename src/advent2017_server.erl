-module(advent2017_server).
-behaviour(gen_server).
-export([start_link/0, stop/0, run_challenge/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).

%%====================================================================
%% Interface
%%====================================================================

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call({global, ?MODULE}, stop).

run_challenge(Challenge) ->
    gen_server:call({global, ?MODULE}, {challenge, Challenge}).

%%====================================================================
%% Callbacks
%%====================================================================
init([]) ->
    {ok, []}.

handle_call({challenge, Challenge}, _From, State) ->
    case Challenge of
        0 -> {reply, {ok, challenge_zero:run()}, State};
        _ -> {reply, {error, "No such challenge was found"}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

% No implementation for these
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.