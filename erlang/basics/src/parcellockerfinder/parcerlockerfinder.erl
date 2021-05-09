-module(parcerlockerfinder).
-author("mzlnk").

%% API
-export([
  randomLocations/1,
  findMyParcelLocker/2,
  findParcelLockersSequentially/2,
  findMyParcelLockerParallel/3,
  findParcelLockersParallel/2,
  findSomeParcelLockersSubparallel/3,
  findParcelLockersSubparallel/2,
  collectData/4,
  compareSpeeds/2
]).

randomLocations(N) -> [{rand:uniform(10001) - 1, rand:uniform(10001) - 1} || _ <- lists:seq(1, N)].


% finding nearest locker for person:

findMyParcelLocker(PersonLocation, LockerLocations) ->
  {PersonX, PersonY} = PersonLocation,
  [First | Tail] =
    lists:sort(
      lists:map(
        fun(Loc) ->
          {LockerX, LockerY} = Loc,
          {math:sqrt(math:pow(PersonX - LockerX, 2) + math:pow(PersonY - LockerY, 2)), Loc}
        end,
        LockerLocations
      )
    ),
  {_, NearestLocker} = First,
  {PersonLocation, NearestLocker}.


% sequential mode:

findParcelLockersSequentially(PersonLocations, LockerLocations) ->
  lists:map(fun(PersonLoc) -> findMyParcelLocker(PersonLoc, LockerLocations) end, PersonLocations).


% parallel mode:

findMyParcelLockerParallel(ParentPid, PersonLocation, LockerLocations) ->
  ParentPid ! findMyParcelLocker(PersonLocation, LockerLocations).

findParcelLockersParallel(PersonLocations, LockerLocations) ->
  CollectPid = spawn(?MODULE, collectData, [self(), 0, length(PersonLocations), []]),

  lists:foreach(fun(PersonLoc) ->
    spawn(?MODULE, findMyParcelLockerParallel, [CollectPid, PersonLoc, LockerLocations]) end, PersonLocations),

  receive
    List -> List
  end.


% subparallel mode:

findSomeParcelLockersSubparallel(CollectPid, PersonLocations, LockerLocations) ->
  lists:foreach(fun(PersonLoc) -> CollectPid ! findMyParcelLocker(PersonLoc, LockerLocations) end, PersonLocations).

findParcelLockersSubparallel(PersonLocations, LockerLocations) ->
  CollectPid = spawn(?MODULE, collectData, [self(), 0, length(PersonLocations), []]),

  {PartCore1, PartCore2} = lists:split(trunc(length(PersonLocations) / 2), PersonLocations),
  spawn(?MODULE, findSomeParcelLockersSubparallel, [CollectPid, PartCore1, LockerLocations]),
  spawn(?MODULE, findSomeParcelLockersSubparallel, [CollectPid, PartCore2, LockerLocations]),

  receive
    List -> List
  end.


% util functions for parallel & subparallel mode:

collectData(ParentPid, Received, All, List) when Received == All -> ParentPid ! List;

collectData(ParentPid, Received, All, List) ->
  receive
    Tuple -> collectData(ParentPid, Received + 1, All, [Tuple | List])
  end.


% compare speeds:

compareSpeeds(PersonLocations, LockerLocations) ->
  {TimeFun1, _} = timer:tc(?MODULE, findParcelLockersSequentially, [PersonLocations, LockerLocations]),
  {TimeFun2, _} = timer:tc(?MODULE, findParcelLockersParallel, [PersonLocations, LockerLocations]),
  {TimeFun3, _} = timer:tc(?MODULE, findParcelLockersSubparallel, [PersonLocations, LockerLocations]),
  io:format("Sequential mode: ~p us ~n", [TimeFun1]),
  io:format("Parallel mode: ~p us ~n", [TimeFun2]),
  io:format("Subparallel mode: ~p us ~n", [TimeFun3]).


