%%%-------------------------------------------------------------------
%% @doc bittorent top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bittorent_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(NodeID) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [NodeID]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([NodeID]) ->
    {ok, { 
        #{
            strategy    => one_for_one,
            intensity   => 1,
            period      => 5
        },
        [
            #{
                id          => NodeID,
                start       => {gen_server, start_link, [
                    {local, tracker},
                    bittorent_tracker,
                    [],
                    []
                ]},
                restart     => permanent,
                shutdown    => brutal_kill,
                type        => worker
            }
        ]
    }}.

%%====================================================================
%% Internal functions
%%====================================================================
