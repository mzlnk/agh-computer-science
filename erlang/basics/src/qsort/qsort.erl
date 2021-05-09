-module(qsort).
-author("mzlnk").

%% API
-export([qs/1, randomElems/3, compareSpeeds/5]).

lessThan(List, Arg) -> [X || X <- List, X < Arg].
grtEqThan(List, Arg) -> [X || X <- List, X >= Arg].

qs([Pivot | Tail]) -> qs(lessThan(Tail, Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail, Pivot));
qs([]) -> [].

randomElems(N, Min, Max) -> [(Min - 1 + rand:uniform(Max - Min + 1)) || _ <- lists:seq(1, N)].

compareSpeeds(List, Module1, Function1, Module2, Function2) ->
  {TimeFun1, _} = timer:tc(Module1, Function1, [List]),
  {TimeFun2, _} = timer:tc(Module2, Function2, [List]),
  io:format("Time 1: ~p us ~n", [TimeFun1]),
  io:format("Time 2: ~p us ~n", [TimeFun2]).