-module(pingpong).
-author("mzlnk").

%% API
-export([start/0, stop/0, play/1, ping/1, pong/1]).

start() ->
  PingPid = spawn(?MODULE, ping, [0]),
  PongPid = spawn(?MODULE, pong, [0]),

  register(ping, PingPid),
  register(pong, PongPid).

stop() ->
  ping ! stop,
  pong ! stop.

play(Count) -> ping ! Count.

ping(PingsReceived) ->
  receive
    stop ->
      io:format("Ping stopped.~n");
    Count when Count > 0 ->
      io:format("Ping! (received: ~p)~n", [PingsReceived + 1]),
      timer:sleep(1000),
      pong ! (Count - 1),
      pingpong:ping(PingsReceived + 1)
  end.

pong(PongsReceived) ->
  receive
    stop ->
      io:format("Pong stopped.~n");
    Count when Count > 0 ->
      io:format("Pong! (received: ~p)~n", [PongsReceived + 1]),
      timer:sleep(1000),
      ping ! (Count - 1),
      pingpong:pong(PongsReceived + 1)
  end.
