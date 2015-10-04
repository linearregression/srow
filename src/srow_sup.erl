%%%-------------------------------------------------------------------
%% @doc srow top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('srow_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Gateway = {srow_gateway,
                {srow_gateway, start_link, [host, port]},
                5000, brutal, worker, []},
    {ok, {one_for_one, [Gateway]}}.

%%====================================================================
%% Internal functions
%%====================================================================
