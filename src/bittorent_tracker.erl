-module(bittorent_tracker).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%====================================================================
%% TYPES
%%====================================================================

%% TODO: Upgrade node connection handling
%% TODO: Enable more than 1 file scenarios
-type peer() :: node().
-type peerlist() :: [peer()].

%%====================================================================
%% API
%%====================================================================

-spec init(any()) -> {ok, []}.
init(_Args) ->
    net_kernel:monitor_nodes(true),
    {ok, []}.

-spec handle_call(get, term(), peerlist()) ->
    {reply, {tracker_response, peerlist()}, peerlist()}.
handle_call(get, _From, Peers) ->
    {reply, {tracker_response, Peers}, Peers};
handle_call(_, _From, _State) ->
    io:format("Tracker: Unexpected message at handle call~n", []),
    {noreply, _State}.

-spec handle_cast(any(), peerlist()) -> {noreply, peerlist()}.
handle_cast(_, _State) ->
    io:format("Tracker: Unexpected message at handle cast~n", []),
    {noreply, _State}.

-spec handle_info({nodeup | nodedown, peer()}, peerlist()) ->
    {noreply, peerlist()}.
handle_info({nodeup, Peer}, Peers) ->
    NewPeers = [Peer | Peers],
    gen_server:multi_call(Peers, peer, {peer_up, Peer}, 0),
    {noreply, NewPeers};
handle_info({nodedown, Peer}, Peers) ->
    NewPeers = lists:delete(Peer, Peers),
    gen_server:multi_call(NewPeers, peer, {peer_down, Peer}, 0),
    {noreply, lists:delete(Peer, Peers)};
handle_info(_, _State) ->
    io:format("Tracker: Unexpected message at handle info~n", []),
    {noreply, _State}.