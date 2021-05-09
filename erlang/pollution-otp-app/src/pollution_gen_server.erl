-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-export([
  addStation/2,
  addValue/4,
  removeValue/3,
  getOneValue/3,
  getStationMean/2,
  getLowestValue/2,
  getDailyMean/2,
  getAmountOfValues/3,
  stop/0,
  crash/0
]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, pollution_gen_server}, ?MODULE, [], []).

init([]) ->
  {ok, pollution:createMonitor()}.

handle_call({getOneValue, Name, Datetime, MeasurementType}, _From, State) ->
  {
    reply,
    pollution:getOneValue(Name, Datetime, MeasurementType, State),
    State
  };

handle_call({getStationMean, Name, MeasurementType}, _From, State) ->
  {
    reply,
    pollution:getStationMean(Name, MeasurementType, State),
    State
  };

handle_call({getLowestValue, Name, MeasurementType}, _From, State) ->
  {
    reply,
    pollution:getLowestValue(Name, MeasurementType, State),
    State
  };

handle_call({getDailyMean, MeasurementType, Datetime}, _From, State) ->
  {
    reply,
    pollution:getDailyMean(MeasurementType, Datetime, State),
    State
  };

handle_call({getAmountOfValues, Name, MeasurementType, Datetime}, _From, State) ->
  {
    reply,
    pollution:getAmountOfValues(Name, MeasurementType, Datetime, State),
    State
  }.

handle_cast({addStation, Name, {X, Y}}, State) ->
  case pollution:addStation(Name, {X, Y}, State) of
    {error, Msg} ->
      io:format("Error: ~p~n", [Msg]),
      {noreply, State};
    NewState ->
      {noreply, NewState}
  end;

handle_cast({addValue, StationNameOrCoords, Datetime, MeasurementType, Value}, State) ->
  case pollution:addValue(StationNameOrCoords, Datetime, MeasurementType, Value, State) of
    {error, Msg} ->
      io:format("Error: ~p~n", [Msg]),
      {noreply, State};
    NewState ->
      {noreply, NewState}
  end;

handle_cast({removeValue, StationNameOrCoords, Datetime, MeasurementType}, State) ->
  case pollution:removeValue(StationNameOrCoords, Datetime, MeasurementType, State) of
    {error, Msg} ->
      io:format("Error: ~p~n", [Msg]),
      {noreply, State};
    NewState ->
      {noreply, NewState}
  end;

handle_cast(stop, State) ->
  {
    stop,
    normal,
    State
  };

handle_cast(crash, State) ->
  {
    noreply,
    1 / 0
  }.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  io:format("Server terminated. Reason: ~p~n", [Reason]),
  Reason.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% User API
%%%===================================================================

addStation(Name, {X, Y}) ->
  gen_server:cast(
    pollution_gen_server,
    {
      addStation,
      Name,
      {X, Y}
    }
  ).

addValue(StationNameOrCoords, Datetime, MeasurementType, Value) ->
  gen_server:cast(
    pollution_gen_server,
    {
      addValue,
      StationNameOrCoords,
      Datetime,
      MeasurementType,
      Value
    }
  ).

removeValue(StationNameOrCoords, Datetime, MeasurementType) ->
  gen_server:cast(
    pollution_gen_server,
    {
      removeValue,
      StationNameOrCoords,
      Datetime,
      MeasurementType
    }
  ).

getOneValue(Name, Datetime, MeasurementType) ->
  gen_server:call(
    pollution_gen_server,
    {
      getOneValue,
      Name,
      Datetime,
      MeasurementType
    }
  ).

getStationMean(Name, MeasurementType) ->
  gen_server:call(
    pollution_gen_server,
    {
      getStationMean,
      Name,
      MeasurementType
    }
  ).

getLowestValue(Name, MeasurementType) ->
  gen_server:call(
    pollution_gen_server,
    {
      getLowestValue,
      Name,
      MeasurementType
    }
  ).

getDailyMean(MeasurementType, Datetime) ->
  gen_server:call(
    pollution_gen_server,
    {
      getDailyMean,
      MeasurementType,
      Datetime
    }
  ).

getAmountOfValues(Name, MeasurementType, Datetime) ->
  gen_server:call(
    pollution_gen_server,
    {
      getAmountOfValues,
      Name,
      MeasurementType,
      Datetime
    }
  ).

stop() ->
  gen_server:cast(pollution_gen_server, stop).

crash() ->
  gen_server:cast(pollution_gen_server, crash).

