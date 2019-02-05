-module(bittorent_peer).
-author("Lövei Péter").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, upload_piece/5,
         handle_info/2, peer/1, seeder/1, generate_torrent_file/0,
         download_next_piece/4, lorem_ipsum/0]).
-export_type([peer/0, piece_index/0, piece_indices/0,
                piece_status/0, download_status/0,
                meta_info/0, torrent_file/0, peer_state/0]).
-compile([{nowarn_unused_function, [
    {check_hash, 2}, 
    {download_piece, 2},
    {read_piece, 3},
    {init_peer, 2}
]}]).

%%====================================================================
%% TYPES
%%====================================================================

-type peer() :: node().
-type piece_index() :: integer().
-type piece_indices() :: [piece_index()].
-type piece_status() :: {piece_index(), [peer()]}.
-type download_status() :: [piece_status()].

-record(meta_info, {
    name            = ""    :: string(),
    piece_length    = 5     :: integer(),
    pieces          = 0     :: [iodata()],
    length          = 0     :: integer()
}).

-record(torrent_file, {
    announce    = tracker       :: atom(),
    info        = #meta_info{}  :: meta_info()
}).

-record(peer_state, {
    meta_info           = #meta_info{}  :: meta_info(),
    peer_list           = []            :: [peer()],
    download_status     = []            :: download_status(),
    already_downloaded  = []            :: piece_indices(),
    to_be_downloaded    = []            :: piece_indices(),
    file                                :: file:fd()
}).

-opaque meta_info()     :: #meta_info{}.
-opaque torrent_file()  :: #torrent_file{}.
-opaque peer_state()    :: #peer_state{}.
        
%%====================================================================
%% API
%%====================================================================

-spec peer(torrent_file()) -> term().
peer(TorrentFile) ->
    {Info, Peers, File, PieceIndices} = init_peer(TorrentFile, true),
    gen_server:start_link({local, peer}, 
                          ?MODULE, 
                          [Info, Peers, File, PieceIndices, []],
                          [{debug, [trace]}]).

-spec seeder(torrent_file()) -> term().
seeder(TorrentFile) ->
    {Info, Peers, File, PieceIndices} = init_peer(TorrentFile, false),
    gen_server:multi_call(Peers, peer, {have_pieces, PieceIndices, node()}, 0),
    gen_server:start_link({local, peer},
                          ?MODULE, 
                          [Info, Peers, File, [], PieceIndices],
                          [{debug, [trace]}]).


%% TODO: Fill record automatically
-spec generate_torrent_file() -> torrent_file().
generate_torrent_file() ->
    #torrent_file{
        announce    = tracker,
        info        = #meta_info{
            name            = "best_joke.txt",
            piece_length    =  10, 
            pieces          = 
                     [<<129,78,149,164,148,191,191,33,119,167,250,10,
                        125,250,84,84,38,88,98,166>>,
                      <<170,119,122,233,187,141,161,10,248,142,37,81,
                        1,84,118,255,191,167,70,11>>,
                      <<165,158,55,94,126,22,60,6,14,197,16,62,
                        97,242,75,240,8,102,26,104>>],
            length          = 3 * 8 + 2
        }
    }.
    
-spec lorem_ipsum() -> torrent_file().
lorem_ipsum() ->
    Fname = "lorem_ipsum.txt",
    File = file:open(Fname, [read]),
    {ok, {file_info, Length,_,_,_,_,_,_,_,_,_,_,_,_}} = file:read_file_info(Fname),
    HashSum = misc:read_file(File, []),
    #torrent_file{
        announce    = tracker,
        info        = #meta_info{
            name            = Fname,
            piece_length    = 10,
            pieces          = HashSum,
            length          = Length
        }
    }.

-spec init(list()) -> {ok, peer_state()} | no_return().
init([Info, PeerList, File, ToBeDownloaded, AlreadyDownloaded]) ->
    {ok, #peer_state{
        meta_info           = Info,
        peer_list           = PeerList,
        to_be_downloaded    = ToBeDownloaded,
        already_downloaded  = AlreadyDownloaded,
        file                = File                     
    }}.

-spec handle_call(term(), term(), peer_state()) ->
    {noreply, peer_state()} | {reply, term(), peer_state()}.
handle_call({peer_down, Peer}, _From, State = #peer_state{peer_list = Peers}) ->
    {noreply, State#peer_state{peer_list = lists:delete(Peer, Peers)}};
handle_call({peer_up, Peer}, _From, State = #peer_state{peer_list = Peers}) ->
    {noreply, State#peer_state{peer_list = [Peer | Peers]}};
handle_call({have_pieces, PieceIndices, Peer}, _From,
            State = #peer_state{meta_info           = Info,
                                download_status     = DownloadStatus,
                                to_be_downloaded    = RemPieces}) ->
    NewStatus = update_piece_status(PieceIndices, Peer, DownloadStatus),
    NewRemPieces = download_random_piece(Info, NewStatus, RemPieces),
    {noreply, State#peer_state{download_status   = NewStatus, 
                               to_be_downloaded  = NewRemPieces}};
handle_call({request, PieceIndex, _Peer}, From, 
            State = #peer_state{meta_info   = Info,
                                file        = File}) ->
    spawn_monitor(?MODULE, upload_piece, [Info, PieceIndex, File, From, self()]),
    {noreply, State};
handle_call(_Request, _From, _State) ->
    io:format("Unexpected message at handle call~n", []),
    {noreply, _State}.

-spec handle_cast(term(), peer_state()) -> 
    {noreply, peer_state()}.
handle_cast({have, PieceIndex, Peer},
            State = #peer_state{download_status = DownloadStatus}) ->
    NewStatus = update_piece_status(PieceIndex, Peer, DownloadStatus),
    {noreply, State#peer_state{download_status = NewStatus}};
handle_cast({uploaded, PieceIndex, Chunk, From}, State) ->
    gen_server:reply(From, {piece, PieceIndex, Chunk}),
    {noreply, State};
handle_cast({downloaded, PieceIndex, Chunk},
            State = #peer_state{meta_info           = Info,
                                peer_list           = Peers, 
                                file                = File,
                                download_status     = DownloadStatus,
                                to_be_downloaded    = RemPieces,
                                already_downloaded  = Downloaded}) ->
    notify_peers(Peers, PieceIndex),
    save_piece(Chunk, file_offset(PieceIndex, Info), File),
    NewRemPieces = download_random_piece(Info, DownloadStatus, RemPieces),
    {noreply, State#peer_state{to_be_downloaded     = NewRemPieces,
                               already_downloaded   = [PieceIndex | Downloaded]}};
handle_cast(_, _State) ->
    io:format("Unexpected message at handle cast~n", []),
    {noreply, _State}.

-spec handle_info(term(), peer_state()) -> {noreply, peer_state()}.
handle_info({'DOWN', _Ref, process, _Pid,
            {{nocatch, {download_failed, PieceIndex}}, _Reason}}, 
            State = #peer_state{to_be_downloaded    = RemPieces,
                                meta_info           = Info,
                                download_status     = DownloadStatus}) ->
    io:format("There was a problem with download~n", []),
    NewRemPieces = [PieceIndex | RemPieces],
    Pieces = download_random_piece(Info, DownloadStatus, NewRemPieces),
    {noreply, State#peer_state{to_be_downloaded = Pieces}};
handle_info(_, _State) ->
    io:format("Unexpected message~n", []),
    {noreply, _State}.


%%====================================================================
%% Internal functions
%%====================================================================

-spec init_peer(torrent_file(), boolean()) ->
    {meta_info(), [peer()], file:fd(), piece_indices()}.
init_peer(#torrent_file{announce = Tracker, info = Info}, IsPeer) ->
    {FileName, Mode} =
        case IsPeer of
            true    -> {atom_to_list(node()), write};
            false   -> {Info#meta_info.name, read}
        end,
    File =
        case file:open(FileName, [Mode]) of
            {ok, IoDevice}  -> IoDevice;
            {error, Reason} -> io:format("Error openning f, reason:~p~n", [Reason])
        end, 
    TrackerNode = misc:get_node_name(Tracker),
    PieceIndices = lists:seq(1, length(Info#meta_info.pieces)),
    net_kernel:connect_node(TrackerNode),
    {tracker_response, Peers} = gen_server:call({Tracker, TrackerNode}, get),
    {Info, Peers, File, PieceIndices}.

-spec upload_piece(meta_info(), piece_index(), file:fd(), peer(), pid()) -> 
    no_return().
upload_piece(Info, PieceIndex, File, DownloadPeer, MonitorPeer) ->
    Chunk = read_piece(file_offset(PieceIndex, Info), 
                        Info#meta_info.piece_length, File),
    gen_server:cast(MonitorPeer, {uploaded, PieceIndex, Chunk, DownloadPeer}).

-spec check_hash(bitstring(), bitstring()) -> boolean().
check_hash(Chunk, HashSum) ->
    crypto:hash(sha, Chunk) =:= HashSum.

-spec download_piece(integer(), peer()) -> iodata() | no_return().
download_piece(PieceIndex, Peer) ->
    try gen_server:call({peer, Peer}, 
                        {request, PieceIndex, node()}, 2000) of
        {piece, PieceIndex, Chunk} -> Chunk;
        _                          -> throw('Error at download_piece')
    catch
        _Class:_Exception ->
            exit({download_failed, PieceIndex})
    end.
    
-spec save_piece(iodata(), integer(), file:fd()) -> ok | no_return().
save_piece(Chunk, Offset, File) ->
    case file:pwrite(File, Offset, Chunk) of
        ok -> ok;
        {error, Reason} -> throw(Reason)
    end.

-spec download_next_piece(piece_index(), bitstring(), peer(), pid()) ->
    term() | no_return().
download_next_piece(PieceIndex, HashSum, DownloadPeer, MonitorPeer) ->
    Chunk = download_piece(PieceIndex, DownloadPeer),
    case check_hash(Chunk, HashSum) of
        true    -> gen_server:cast(MonitorPeer, {downloaded, PieceIndex, Chunk});
        false   -> throw({download_failed, PieceIndex})
    end.

-spec select_next_piece(download_status(), piece_indices()) ->
    [{piece_index(), peer(), piece_indices()}] | [].
select_next_piece(_, []) -> [];
select_next_piece(DownloadStatus, RemPieces) ->
    PieceIndex = misc:get_random_item(RemPieces),
    case proplists:get_value(PieceIndex, DownloadStatus) of
        undefined   -> [];
        Peers       ->
            Peer = misc:get_random_item(Peers),
            [{PieceIndex, Peer, lists:delete(PieceIndex, RemPieces)}]
    end.

-spec file_offset(piece_index(), meta_info()) -> integer() | no_return().
file_offset(PieceIndex, #meta_info{piece_length = PieceLength,
                                   length       = FileLength}) ->
    Offset = (PieceIndex - 1) * PieceLength,
    case Offset =< FileLength of
        true    -> Offset;
        false   -> throw(offset_error)
    end.


-spec download_random_piece(meta_info(), download_status(), piece_indices()) -> 
    piece_indices().
download_random_piece(#meta_info{pieces = Pieces}, DownloadStatus, RemPieces) ->
    case select_next_piece(DownloadStatus, RemPieces) of
        []                              -> RemPieces;
        [{PieceIndex, Peer, NewRemPieces}] -> 
            HashSum = lists:nth(PieceIndex, Pieces),
            spawn_monitor(?MODULE, download_next_piece,
                          [PieceIndex, HashSum, Peer, self()]),
            NewRemPieces
    end.

-spec notify_peers([peer()], piece_index()) -> list().
notify_peers(Peers, PieceIndex) ->
    gen_server:multi_call(Peers, peer, {have, PieceIndex, node()}, 0).

-spec read_piece(integer(), integer(), file:fd()) -> 
    string() | binary() | no_return().
read_piece(Offset, PieceLength, File) ->
    case file:pread(File, Offset, PieceLength) of
        {ok, Chunk}     -> Chunk;
        eof             -> throw(read_piece_error);
        {error, Reason} -> throw(Reason)
    end.

-spec update_piece_status(piece_index() | piece_indices(),
                          peer(), download_status()) ->
    download_status().
update_piece_status(PieceIndex, Peer, DownloadStatus)
when not is_list(PieceIndex) ->
    Peers =
        case proplists:get_value(PieceIndex, DownloadStatus) of
            undefined -> [];
            Ps        -> Ps
        end,
    [{PieceIndex, [Peer | Peers]} | proplists:delete(PieceIndex, DownloadStatus)];
update_piece_status(PieceIndices, Peer, DownloadStatus) ->
    lists:foldr(
        fun(PieceIndex, Status) -> 
            update_piece_status(PieceIndex, Peer, Status)
        end,
        DownloadStatus,
        PieceIndices
    ).