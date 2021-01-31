-module(tracker).
-export([main/0, loop/1]).

main() -> Pid = spawn(tracker, loop, [[]]),
    register(tracker, Pid).

addPeerToList(Peer, []) -> erlang:monitor(process, Peer), [Peer];
addPeerToList(Peer, [Peer|Tail]) -> [Peer|Tail];
addPeerToList(Peer, [OtherPeer|Tail]) -> [OtherPeer|addPeerToList(Peer, Tail)].

removePeerFromList(_, []) -> [];
removePeerFromList(Peer, [Peer|Tail]) -> removePeerFromList(Peer, Tail);
removePeerFromList(Peer, [OtherPeer|Tail]) -> [OtherPeer|removePeerFromList(Peer, Tail)].

loop(Peers) -> receive
        {get, Peer} -> Peer ! {tracker_response, Peers}, loop(addPeerToList(Peer, Peers));
        {'DOWN', _, _, _, Peer, _} -> loop(removePeerFromList(Peer, Peers));
        OtherMessage -> io:format("Unexpected message: ~w~n", [OtherMessage])
    end.