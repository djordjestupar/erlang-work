-module(example).
-export([main/0, register_tracker/0]).    

best_joke() ->
    string:join([lists:duplicate(8, C) || C <- [$a, $b, $c]], "\n").

write_best_joke_to_file(File) ->
    {ok, Fd} = file:open(File, [write]),
    file:write(Fd, [best_joke()]),
    file:close(Fd).

torrent(Name) ->
    maps:from_list([
        {announce, {tracker,'tracker@127.0.0.1'}},
        {info, maps:from_list([
            {name, Name},
            {piece_length, 10}, 
            {pieces, [
                    <<129,78,149,164,148,191,191,33,119,167,250,10,125,250,84,84,38,88,98,166>>,
                    <<170,119,122,233,187,141,161,10,248,142,37,81,1,84,118,255,191,167,70,11>>,
                    <<165,158,55,94,126,22,60,6,14,197,16,62,97,242,75,240,8,102,26,104>>
                ]},
            {length, 3 * 8 + 2}
        ])}
    ]).

register_tracker() -> tracker:main().

main() ->
    Name1 = "files\\seeder1.txt",
    write_best_joke_to_file(Name1),
    Name2 = "files\\seeder2.txt",
    write_best_joke_to_file(Name2),
    Name3 = "files\\peer1.txt",
    Name4 = "files\\peer2.txt",
    Name5 = "files\\peer3.txt",
    peer:seeder(torrent(Name1)),
    peer:seeder(torrent(Name2)),
    peer:peer(torrent(Name3)),
    peer:peer(torrent(Name4)),
    peer:peer(torrent(Name5)).