%%%-------------------------------------------------------------------
%% @doc advent2017 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(advent2017_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
-spec start_link() -> ignore | {error, _} | {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init(any()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, { {one_for_all, 0, 1}, 
         [{advent2017,
           {advent2017_server, start_link, []},
           permanent,
           5000,
           worker,
           [advent2017_server]
         }]}}.

%%====================================================================
%% Internal functions
%%====================================================================
