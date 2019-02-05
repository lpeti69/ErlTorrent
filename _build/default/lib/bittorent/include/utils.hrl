-module(utils).
-author("Lövei Péter").

-record(torrent_file, {
    announce    = {tracker, "tracker@Palacsinta"}, %% hostname needs to be specified
    info        = #meta_info{}
}).

-record(meta_info, {
    name            = "", %% the filename
    piece_length    = 10, %% the number of bytes a piece consists of
    pieces          = 0, %% the hash sums of the pieces
    length          = 0 %% the file size in terms of bytes
}).

