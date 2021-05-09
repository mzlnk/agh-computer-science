-module(pollution_server).
-author("mzlnk").

%% API
-export([
  start/0,
  stop/0,
  addStation/2,
  addValue/4,
  removeValue/3,
  getOneValue/3,
  getStationMean/2,
  getLowestValue/2,
  getDailyMean/2,
  getAmountOfValues/3
]).

-export([init/0]).

start() -> register(pollutionServer, spawn(pollution_server, init, [])).

stop() -> pollutionServer ! stop.

init() ->
  Monitor = pollution:createMonitor(),
  loop(Monitor).

loop(Monitor) ->
  receive
    stop -> ok;

    {Pid, addStation, Name, {X, Y}} ->
      case pollution:addStation(Name, {X, Y}, Monitor) of
        {error, Msg} ->
          Pid ! {error, Msg},
          loop(Monitor);
        UpdatedMonitor ->
          Pid ! ok,
          loop(UpdatedMonitor)
      end;

    {Pid, addValue, StationNameOrCoords, Datetime, MeasurementType, Value} ->
      case pollution:addValue(StationNameOrCoords, Datetime, MeasurementType, Value, Monitor) of
        {error, Msg} ->
          Pid ! {error, Msg},
          loop(Monitor);
        UpdatedMonitor ->
          Pid ! ok,
          loop(UpdatedMonitor)
      end;

    {Pid, removeValue, StationNameOrCoords, Datetime, MeasurementType} ->
      case pollution:removeValue(StationNameOrCoords, Datetime, MeasurementType, Monitor) of
        {error, Msg} ->
          Pid ! {error, Msg},
          loop(Monitor);
        UpdatedMonitor ->
          Pid ! ok,
          loop(UpdatedMonitor)
      end;

    {Pid, getOneValue, Name, Datetime, MeasurementType} ->
      Value = pollution:getOneValue(Name, Datetime, MeasurementType, Monitor),
      Pid ! Value,
      loop(Monitor);

    {Pid, getStationMean, Name, MeasurementType} ->
      Result = pollution:getStationMean(Name, MeasurementType, Monitor),
      Pid ! Result,
      loop(Monitor);

    {Pid, getLowestValue, Name, MeasurementType} ->
      Result = pollution:getLowestValue(Name, MeasurementType, Monitor),
      Pid ! Result,
      loop(Monitor);

    {Pid, getDailyMean, MeasurementType, Datetime} ->
      Result = pollution:getDailyMean(MeasurementType, Datetime, Monitor),
      Pid ! Result,
      loop(Monitor);

    {Pid, getAmountOfValues, Name, MeasurementType, Datetime} ->
      Result = pollution:getAmountOfValues(Name, MeasurementType, Datetime, Monitor),
      Pid ! Result,
      loop(Monitor)
  end.

addStation(Name, {X, Y}) ->
  pollutionServer ! {self(), addStation, Name, {X, Y}},
  receive
    V -> V
  end.

addValue(StationNameOrCoords, Datetime, MeasurementType, Value) ->
  pollutionServer ! {self(), addValue, StationNameOrCoords, Datetime, MeasurementType, Value},
  receive
    V -> V
  end.

removeValue(StationNameOrCoords, Datetime, MeasurementType) ->
  pollutionServer ! {self(), removeValue, StationNameOrCoords, Datetime, MeasurementType},
  receive
    V -> V
  end.

getOneValue(Name, Datetime, MeasurementType) ->
  pollutionServer ! {self(), getOneValue, Name, Datetime, MeasurementType},
  receive
    V -> V
  end.

getStationMean(Name, MeasurementType) ->
  pollutionServer ! {self(), getStationMean, Name, MeasurementType},
  receive
    V -> V
  end.

getLowestValue(Name, MeasurementType) ->
  pollutionServer ! {self(), getLowestValue, Name, MeasurementType},
  receive
    V -> V
  end.

getDailyMean(MeasurementType, Datetime) ->
  pollutionServer ! {self(), getDailyMean, MeasurementType, Datetime},
  receive
    V -> V
  end.

getAmountOfValues(Name, MeasurementType, Datetime) ->
  pollutionServer ! {self(), getAmountOfValues, Name, MeasurementType, Datetime},
  receive
    V -> V
  end.



