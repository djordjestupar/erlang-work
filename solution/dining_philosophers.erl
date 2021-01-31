-module(dining_philosophers).
-export([philosopher/2, waiter/3, start/0]).


start() -> Num = 5, 
        Waiter = spawn(dining_philosophers, waiter, [initPhilState(Num - 1), 10, Num]),
        spawnPhilosophers(Num - 1, Waiter).


initPhilState(0) -> [{0, thinking, 0}];
initPhilState(N) -> [{N, thinking, 0}|initPhilState(N - 1)].

spawnPhilosophers(0, Waiter) -> spawn(dining_philosophers, philosopher, [0, Waiter]);
spawnPhilosophers(N, Waiter) -> spawn(dining_philosophers, philosopher, [N, Waiter]),
                                spawnPhilosophers(N - 1, Waiter).

philosopher(Id, Waiter) -> 
        think(),
        takeForks(Id, Waiter),
        eat(Id),
        putForks(Id, Waiter),
        philosopher(Id, Waiter).


think() -> timer:sleep(random(500, 5000)).
eat(Id) -> io:format("Philosopher ~w started to eat.~n", [Id]), 
        timer:sleep(random(50, 500)),
        io:format("Philosopher ~w finished with eating.~n", [Id]).

random(A, B) -> A + rand:uniform(B - A).

takeForks(Id, Waiter) ->
    Waiter ! {take, Id, self()},
    receive
        wait -> timer:sleep(random(50, 500)), takeForks(Id, Waiter);
        ok -> ok
    end.

putForks(Id, Waiter) ->
    Waiter ! {put, Id}.

%PhilState is list of elements of form {id, state, waitCount}
%State can be thinking, eating, hungry and starving
waiter(PhilState, MaxWaitingCount, Num) ->
    receive
        {take, Id, Philosopher} -> case canEat(PhilState, Id, Num) of
                                        true -> Philosopher ! ok, waiter(takeForksInState(PhilState, Id), MaxWaitingCount, Num);
                                        false -> Philosopher ! wait, waiter(updateWait(PhilState, Id, MaxWaitingCount), MaxWaitingCount, Num)
                                    end;
        {put, Id} -> waiter(putForksInState(PhilState, Id), MaxWaitingCount, Num)
    end.

getState([], _) -> false;
getState([{Id, S, W}|_], Id) -> {Id, S, W};
getState([_|T], Id) -> getState(T, Id).

mod(X, Y) when X > 0 -> X rem Y;
mod(X, Y) when X < 0 -> Y + X rem Y;
mod(0, _) -> 0.

canEat(PhilState, Id, Num) -> 
    {_, S, _} = getState(PhilState, Id),
    {_, SL, _} = getState(PhilState, mod(Id - 1, Num)),
    {_, SR, _} = getState(PhilState, mod(Id + 1, Num)),
    (SL /= eating andalso SR /= eating) andalso
    (S == starving orelse (SL /= starving andalso SR /= starving)).

takeForksInState([], _) -> [];
takeForksInState([{Id, _, _}|T], Id) -> [{Id, eating, 0}|T];
takeForksInState([H|T], Id) -> [H|takeForksInState(T, Id)].

putForksInState([], _) -> [];
putForksInState([{Id, eating, _}|T], Id) -> [{Id, thinking, 0}|T];
putForksInState([H|T], Id) -> [H|putForksInState(T, Id)].

updateWait([], _, _) -> [];
updateWait([{Id, thinking, _}|T], Id, _) -> [{Id, hungry, 0}|T];
updateWait([{Id, hungry, W}|T], Id, MaxWaitingCount) when W + 1 > MaxWaitingCount -> [{Id, starving, W + 1}|T];
updateWait([{Id, State, W}|T], Id, _) -> [{Id, State, W + 1}|T];
updateWait([H|T], Id, MaxWaitingCount) -> [H|updateWait(T, Id, MaxWaitingCount)].