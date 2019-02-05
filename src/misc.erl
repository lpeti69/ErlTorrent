-module(misc).
-export([get_node_name/1, get_random_item/1, read_file/2]).

-spec get_node_name(atom()) -> node().
get_node_name(Sname) ->
    Name = atom_to_list(Sname) ++ "@" ++ net_adm:localhost(),
    list_to_atom(Name).

-spec get_random_item(list()) -> term(). 
get_random_item(List) ->
    lists:nth(rand:uniform(length(List)), List).

-spec read_file(file:fd(), list()) -> list() | no_return().
read_file(File, Acc) ->
    case file:read(File, 10) of
        {ok, Chunk}     -> read_file(File, [crypto:hash(sha, Chunk) | Acc]);
        eof             -> Acc;
        {error, Reason}  -> throw(Reason)
    end.