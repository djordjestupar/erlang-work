-module(producer_consumer).
-export([producer/1, consumer/1, buffer/1, start/0]).

start() -> Capacity = 10,  
        Buffer = spawn(producer_consumer, buffer, [Capacity]),
        spawn(producer_consumer, producer, [Buffer]),
        spawn(producer_consumer, consumer, [Buffer]).

producer(Buffer) ->
    Item = produce(),
    Buffer ! {put, Item, self()},
    receive
        ok -> producer(Buffer);
        bufferIsFull -> waitProducer(Item, Buffer), producer(Buffer)
    end.

waitProducer(Item, Buffer) -> 
    receive
        bufferIsNotFull -> Buffer ! {put, Item, self()}, waitProducer(Item, Buffer);
        bufferIsFull -> waitProducer(Item, Buffer);
        ok -> producer(Buffer)
    end.

produce() -> timer:sleep(random(500, 5000)),
        Item = random(0, 20),
        io:format('Item ~w produced.~n', [Item]),
        Item.

%Returns random int between A and B
random(A, B) -> A + rand:uniform(B - A).

consumer(Buffer) ->
    Buffer ! {get, self()},
    receive
        {Item, ok} -> consume(Item), consumer(Buffer);
        bufferIsEmpty -> waitConsumer(Buffer)
    end.

waitConsumer(Buffer) -> 
    receive
        bufferIsNotEmpty -> consumer(Buffer)
    end.


add([], E) -> [E];
add([E|T], E) -> T;
add([H|T], E) -> [H|add(T, E)].

sendToConsumers(_, []) -> [];
sendToConsumers(0, L) -> L;
sendToConsumers(N, [H|T]) -> H ! bufferIsNotEmpty, 
                            sendToConsumers(N - 1, T).


sendToProducers(_, []) -> [];
sendToProducers(0, L) -> L;
sendToProducers(N, [H|T]) -> H ! bufferIsNotFull, 
                            sendToProducers(N - 1, T).

consume(Item) -> 
    timer:sleep(random(500, 5000)),
    io:format('Item ~w consumed.~n', [Item]).


buffer(Capacity) -> buffer(Capacity, 0, [], [], []).

putAtEnd(Item, []) -> [Item];
putAtEnd(Item, [H|T]) -> [H|putAtEnd(Item, T)].

buffer(RemSize, NumOfBuffered, BufferedItems, WaitingProducers, WaitingConsumers) ->
    receive
        {put, _, Producer} when RemSize == 0 -> 
            Producer ! bufferIsFull,
            buffer(RemSize, NumOfBuffered, BufferedItems, add(WaitingProducers, Producer), WaitingConsumers);
        
        {put, Item, Producer} when RemSize > 0 -> 
            Producer ! ok,
            buffer(RemSize - 1, NumOfBuffered + 1, putAtEnd(Item, BufferedItems), WaitingProducers, 
                sendToConsumers(NumOfBuffered + 1, WaitingConsumers));
        
        {get, Consumer} when NumOfBuffered == 0 ->
            Consumer ! bufferIsEmpty,
            buffer(RemSize, NumOfBuffered, BufferedItems, WaitingProducers, add(WaitingConsumers, Consumer));
        
        {get, Consumer} when NumOfBuffered > 0 -> 
            [Item | BufferedItems1] = BufferedItems,
            Consumer ! {Item, ok},
            buffer(RemSize + 1, NumOfBuffered - 1, BufferedItems1, 
                sendToProducers(RemSize + 1, WaitingProducers), WaitingConsumers);
        
        _ -> buffer(RemSize, NumOfBuffered, BufferedItems, WaitingProducers, WaitingConsumers)
    end.