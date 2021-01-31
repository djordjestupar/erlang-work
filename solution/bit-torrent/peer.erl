-module(peer).
-export([peer/1, seeder/1, peer_common/3, dowlnload_piece_proccess/6, upload_piece/4, contains/2]).

check_hash(PieceOfFile, HashSum) -> crypto:hash(sha, PieceOfFile) == HashSum.

download_piece(PieceIndex, PeerPid) -> PeerPid ! {request, PieceIndex, self()},
    receive
        {piece, PieceIndex, Chunk} -> Chunk
        after
            2000 -> exit({download_failed, PieceIndex})
    end.

save_piece(Chunk, Offset, FileObject) -> file:pwrite(FileObject, Offset, Chunk).

download_next_piece(PieceIndex, HashSum, Peer) -> Chunk = download_piece(PieceIndex, Peer),
    case check_hash(Chunk, HashSum) of
        true -> Chunk;
        false -> exit({{download_failed, PieceIndex}})
    end.

contains([], _) -> false;
contains([{_,[]}|Tail], Elem) -> contains(Tail, Elem);
contains([{Elem,_}|_], Elem) -> true;
contains([_|Tail], Elem) -> contains(Tail, Elem).

filterRemainingPieces(DownloadStatus, RemainingPieces) -> lists:filter(fun(X) -> peer:contains(DownloadStatus, X) end, RemainingPieces).

findPeer(_, []) -> exit(fail);
findPeer(Elem, [{Elem, [Peer|_]}|_]) -> Peer;
findPeer(Elem, [_|Tail]) -> findPeer(Elem, Tail).

select_next_piece(DownloadStatus, RemainingPieces) -> RemainingPiecesInDownloadStatus = filterRemainingPieces(DownloadStatus, RemainingPieces),
    case RemainingPiecesInDownloadStatus of
        [] -> [];
        _ -> Length = length(RemainingPiecesInDownloadStatus), 
            RandomIndex = rand:uniform(Length), 
            Element = lists:nth(RandomIndex, RemainingPiecesInDownloadStatus),
            Peer = findPeer(Element, DownloadStatus),
            [{Element, Peer, RemainingPieces}]
    end.

file_offset(Index, MetaInfo) -> Info = maps:get(info, MetaInfo),
    PieceLenght = maps:get(piece_length, Info), 
    (Index - 1) * PieceLenght.

download_random_piece(_, _, [], _) -> [];
download_random_piece(MetaInfo, DownloadStatus, RemainingPieces, FileObject) -> [{Element, Peer, RemainingPieces}] = select_next_piece(DownloadStatus, RemainingPieces),
    Info = maps:get(info, MetaInfo),
    Hashes = maps:get(pieces, Info), 
    HashSum = lists:nth(Element, Hashes),
    Pid = spawn(peer, dowlnload_piece_proccess, [Element, HashSum, Peer, self(), FileObject, MetaInfo]),
    erlang:monitor(process, Pid),
    lists:delete(Element, RemainingPieces).

dowlnload_piece_proccess(Element, HashSum, Peer, ParentId, FileObject, MetaInfo) -> 
    Chunk = download_next_piece(Element, HashSum, Peer),
    save_piece(Chunk, file_offset(Element, MetaInfo), FileObject),
    ParentId ! {downloaded, Element}.

notify_peers([], _) -> ok;
notify_peers([Head|Tail], PieceIndex) when Head /= self() -> Head ! {have, PieceIndex, self()},
    notify_peers(Tail, PieceIndex);
notify_peers([_|Tail], PieceIndex) -> notify_peers(Tail, PieceIndex).

read_piece(FileOffset, PieceSize, FileObject) -> {ok, Chunk} = file:pread(FileObject, FileOffset, PieceSize), Chunk.

add_peer(Peer, []) -> [Peer];
add_peer(Peer, [Peer|List]) -> [Peer|List];
add_peer(Peer, [OtherPeer|List]) -> [OtherPeer|add_peer(Peer, List)].

update_piece_status(PieceIndex, Peer, []) -> [{PieceIndex, [Peer]}];
update_piece_status(PieceIndex, Peer, [{PieceIndex,List}|Tail]) -> [{PieceIndex, add_peer(Peer, List)}|Tail];
update_piece_status(PieceIndex, Peer, [Head|Tail]) -> [Head|update_piece_status(PieceIndex, Peer, Tail)].

update_pieces_status([], _, DownloadStatus) -> DownloadStatus;
update_pieces_status([PieceIndex|Rest], Peer, DownloadStatus) -> update_pieces_status(Rest, Peer, update_piece_status(PieceIndex, Peer, DownloadStatus)).

upload_piece(PieceIndex, Peer, FileObject, MetaInfo) -> Offset = file_offset(PieceIndex, MetaInfo),
    Info = maps:get(info, MetaInfo),
    PieceSize = maps:get(piece_length, Info),
    Chunk = read_piece(Offset, PieceSize, FileObject),
    Peer!{piece, PieceIndex, Chunk}.

update_download_status([], _) -> [];
update_download_status([{Element, List}|Tail], PeersList) -> [{Element, lists:filter(fun(X) -> lists:member(X, PeersList) end, List)}|update_download_status(Tail, PeersList)].

notify_have_pieces([], _) -> ok;
notify_have_pieces(_, []) -> ok;
notify_have_pieces(Pieces, [Peer|Other]) when Peer /= self() -> Peer ! {have_pieces, Pieces, self()}, 
    notify_have_pieces(Pieces, Other);
notify_have_pieces(Pieces, [_|Other]) -> notify_have_pieces(Pieces, Other).

get_file_name(MetaInfo) -> Info = maps:get(info, MetaInfo),
   maps:get(name, Info).

is_downloaded(DownloadedPieces, MetaInfo) -> Info = maps:get(info, MetaInfo),
    PieceSize = maps:get(piece_length, Info),
    FileSize = maps:get(length, Info),
    length(create_full_list(PieceSize, FileSize, 1)) == length(DownloadedPieces).

loop(MetaInfo, PeersList, DownloadStatus, DownloadedPieces, RemainingPieces, FileObject, IsLeecher) -> 
    case IsLeecher andalso is_downloaded(DownloadedPieces, MetaInfo) of
        true -> io:format("File ~s successfully downloaded.~n", [get_file_name(MetaInfo)]), receive_fun(MetaInfo, PeersList, DownloadStatus, DownloadedPieces, RemainingPieces, FileObject, false);
        false -> receive_fun(MetaInfo, PeersList, DownloadStatus, DownloadedPieces, RemainingPieces, FileObject, IsLeecher)
    end.

receive_fun(MetaInfo, PeersList, DownloadStatus, DownloadedPieces, RemainingPieces, FileObject, IsLeecher) ->
    receive
        {tracker_response, Peers} -> notify_have_pieces(DownloadedPieces, Peers), loop(MetaInfo, Peers, update_download_status(DownloadStatus, Peers), DownloadedPieces, RemainingPieces, FileObject, IsLeecher);
        {have_pieces, PieceIndices, Peer} -> NewDownloadStatus = update_pieces_status(PieceIndices, Peer, DownloadStatus), 
            Peer ! {have_pieces_response, DownloadedPieces, self()},
            NewRemainingPieces = download_random_piece(MetaInfo, NewDownloadStatus, RemainingPieces, FileObject),
            loop(MetaInfo, PeersList, NewDownloadStatus, DownloadedPieces, NewRemainingPieces, FileObject, IsLeecher);
        {have_pieces_response, PieceIndices, Peer} -> NewDownloadStatus = update_pieces_status(PieceIndices, Peer, DownloadStatus),
            NewRemainingPieces = download_random_piece(MetaInfo, NewDownloadStatus, RemainingPieces, FileObject),
            loop(MetaInfo, PeersList, NewDownloadStatus, DownloadedPieces, NewRemainingPieces, FileObject, IsLeecher);
        {have, PieceIndex, Peer} -> loop(MetaInfo, PeersList, update_piece_status(PieceIndex, Peer, DownloadStatus), DownloadedPieces, RemainingPieces, FileObject, IsLeecher);
        {request, PieceIndex, Peer} -> spawn(peer, upload_piece, [PieceIndex, Peer, FileObject, MetaInfo]),
            loop(MetaInfo, PeersList, DownloadStatus, DownloadedPieces, RemainingPieces, FileObject, IsLeecher);
        {downloaded, PieceIndex} -> notify_peers(PeersList, PieceIndex),
            NewDownloadedPieces = [PieceIndex|DownloadedPieces],
            NewRemainingPieces = download_random_piece(MetaInfo, DownloadStatus, RemainingPieces, FileObject),
            loop(MetaInfo, PeersList, DownloadStatus, NewDownloadedPieces, NewRemainingPieces, FileObject, IsLeecher);
        {'DOWN', _, _, _, {download_failed, PieceIndex}} -> FirstRemainingPieces = [PieceIndex|RemainingPieces], 
            SecondRemainingPieces = download_random_piece(MetaInfo, DownloadStatus, FirstRemainingPieces, FileObject),
            loop(MetaInfo, PeersList, DownloadStatus, DownloadedPieces, SecondRemainingPieces, FileObject, IsLeecher)
    end.

create_full_list(PieceSize, FileSize, Number) when PieceSize >= FileSize -> [Number];
create_full_list(PieceSize, FileSize, Number) -> [Number|create_full_list(PieceSize, FileSize - PieceSize, Number + 1)].

peer_common(MetaInfo, Fun, IsLeecher) ->
    Info = maps:get(info, MetaInfo),
    {RegisteredName, _} = maps:get(announce, MetaInfo),
    Name = maps:get(name, Info),
    PieceSize = maps:get(piece_length, Info),
    FileSize = maps:get(length, Info),
    {ok, FileObject} = file:open(Name, [read, write]),
    {DownloadedPieces, RemainingPieces} = Fun(create_full_list(PieceSize, FileSize, 1)),
    RegisteredName ! {get, self()},
    receive
        {tracker_response, Peers} -> 
            timer:send_interval(2000, RegisteredName, {get, self()}),
            loop(MetaInfo, Peers, [], DownloadedPieces, RemainingPieces, FileObject, IsLeecher)
    end.

peer(MetaInfo) -> spawn(peer, peer_common, [MetaInfo, fun(X) -> {[], X} end, true]).
seeder(MetaInfo) -> spawn(peer, peer_common, [MetaInfo, fun(X) -> {X, []} end, false]).