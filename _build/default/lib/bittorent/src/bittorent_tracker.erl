-module(bittorent_tracker).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-type peerlist() :: [{pid(), reference()}].
-record(state, {
    peerlist = [] :: peerlist()
}).

-spec init(term()) -> {ok, []}.
init(_Args) ->
    {ok, #state{}}.

-spec handle_call(get, pid(), State :: #state{}) ->
    {reply, {ok, Reply :: peerlist()}, NewState :: #state{}}.
handle_call(get, PeerPid, State = #state{peerlist = Peers}) ->
    case proplists:get_value(PeerPid, Peers) of
        undefined   ->
            PeerRef = erlang:monitor(PeerPid),
            NewPeers = [{PeerPid, PeerRef} | Peers],
            {reply, {ok, proplists:get_keys(NewPeers)},
             #state{peerlist = NewPeers}};
        _           ->
            {reply, proplists:get_keys(Peers), State}
    end.

-spec handle_cast(term(), #state{}) -> {term(), #state{}}.
handle_cast(_, _State) ->
    {noreply, _State}.

-spec handle_info(Info :: term(), #state{}) -> {term(), #state{}}.
handle_info({'DOWN', Ref, process, PeerPid, _Reason},
            State = #state{peerlist = Peers}) ->
    case proplists:lookup(PeerPid, Peers) of
        {PeerPid, Ref} ->
            erlang:demonitor(Ref),
            {noreply, proplists:delete(PeerPid, Peers)};
        none           ->
            {noreply, State}
    end;

handle_info(_, _State) ->
    io:format("Unexpected message~n", []),
    {noreply, _State}.
