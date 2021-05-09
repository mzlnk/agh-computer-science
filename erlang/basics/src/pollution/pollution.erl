-module(pollution).
-author("mzlnk").

%% API
-export([
  createMonitor/0,
  addStation/3,
  addValue/5,
  removeValue/4,
  getOneValue/4,
  getStationMean/3,
  getLowestValue/3,
  getDailyMean/3,
  getAmountOfValues/4
]).

-type measurementType() :: pm25 | pm10 | temperature.

-record(coords, {x = 0 :: integer(), y = 0 :: integer()}).
-record(monitor, {stations = dict:new(), stationCoords = dict:new()}).
-record(station, {name, coords = #coords{}, measurements = dict:new()}).
-record(measurement, {type :: measurementType(), datetime :: calendar:datetime(), value :: float()}).

% funkcja tworząca pusty monitor:

createMonitor() -> #monitor{}.


% funkcja pobierająca stację o danej nazwie/koordynatach z monitora:

getStation({X, Y}, Monitor) ->
  case dict:find({X, Y}, Monitor#monitor.stationCoords) of
    error -> error;
    {ok, [StationName | _]} -> getStation(StationName, Monitor)
  end;

getStation(Name, Monitor) ->
  case dict:find(Name, Monitor#monitor.stations) of
    error -> error;
    {ok, Value} ->
      [E | _] = Value,
      E
  end.


% funkcja sprawdzająca czy dana stacja istnieje:

stationExists(Name, {X, Y}, Monitor) ->
  stationExists(Name, Monitor) or stationExists({X, Y}, Monitor).

stationExists({X, Y}, Monitor) ->
  dict:is_key({X, Y}, Monitor#monitor.stationCoords);

stationExists(Name, Monitor) ->
  dict:is_key(Name, Monitor#monitor.stations).


% funkcja dodająca nową stację:

addStation(Name, {X, Y}, Monitor) ->
  case stationExists(Name, {X, Y}, Monitor) of
    true ->
      {error, "Station with given name or coords already exists"};
    false ->
      NewStations = dict:append(Name, #station{name = Name, coords = #coords{x = X, y = Y}, measurements = dict:new()}, Monitor#monitor.stations),
      NewStationCoords = dict:append({X, Y}, Name, Monitor#monitor.stationCoords),
      Monitor#monitor{stations = NewStations, stationCoords = NewStationCoords}
  end.

% pomocnicza funkcja sprawdzająca, czy można dodać pomiar:

canAddValue({X, Y}, Datetime, MeasurementType, Monitor) ->
  Station = getStation({X, Y}, Monitor),

  dict:is_key(
    {X, Y},
    Monitor#monitor.stationCoords
  )
    andalso
    not dict:is_key(
      {Datetime, MeasurementType},
      Station#station.measurements);

canAddValue(Name, Datetime, MeasurementType, Monitor) ->
  Station = getStation(Name, Monitor),

  dict:is_key(
    Name,
    Monitor#monitor.stations
  )
    andalso
    not dict:is_key(
      {Datetime, MeasurementType},
      Station#station.measurements
    ).

% pomocnicza funkcja dodająca pomiar do monitora (wydzielona, aby obsłużyć dodawanie przez koordynaty jak i nazwy stacji):

addValueToMonitor(Name, Datetime, MeasurementType, Value, Monitor) ->
  Monitor#monitor{
    stations = dict:update(
      Name,
      fun([Station | _]) ->
        [Station#station{
          measurements = dict:append(
            {Datetime, MeasurementType},
            #measurement{
              type = MeasurementType,
              datetime = Datetime,
              value = Value},
            Station#station.measurements)
        }]
      end,
      Monitor#monitor.stations
    )
  }.

% funkcja dodająca nowy pomiar:

addValue({X, Y}, Datetime, MeasurementType, Value, Monitor) ->
  case canAddValue({X, Y}, Datetime, MeasurementType, Monitor) of
    true ->
      Station = getStation({X, Y}, Monitor),
      addValueToMonitor(Station#station.name, Datetime, MeasurementType, Value, Monitor);
    false ->
      {error, "Measurement with given datetime and type already exists in station"}
  end;

addValue(Name, Datetime, MeasurementType, Value, Monitor) ->
  case canAddValue(Name, Datetime, MeasurementType, Monitor) of
    true ->
      addValueToMonitor(Name, Datetime, MeasurementType, Value, Monitor);
    false ->
      {error, "Cannot add measurement"}
  end.

% funkcja pomocnicza sprawdzająca, czy można usunąć dany pomiar:

canRemoveValue(Name, Datetime, MeasurementType, Monitor) ->
  Station = getStation(Name, Monitor),

  dict:is_key(Name, Monitor#monitor.stations)
    andalso
    dict:is_key({Datetime, MeasurementType}, Station#station.measurements);

canRemoveValue({X, Y}, Datetime, MeasurementType, Monitor) ->
  Station = getStation({X, Y}, Monitor),

  dict:is_key({X, Y}, Monitor#monitor.stations)
    andalso
    dict:is_key({Datetime, MeasurementType}, Station#station.measurements).


% pomocnicza funkcja usuwająca pomiar z monitora (wydzielona, aby obsłużyć dodawanie przez koordynaty jak i nazwy stacji):

removeValueFromMonitor(Name, Datetime, MeasurementType, Monitor) ->
  Monitor#monitor{
    stations = dict:update(
      Name,
      fun([Station | _]) -> [Station#station{
        measurements = dict:erase({Datetime, MeasurementType}, Station#station.measurements)
      }]
      end,
      Monitor#monitor.stations
    )
  }.


% funkcja usuwająca pomiar z monitora:

removeValue(Name, Datetime, MeasurementType, Monitor) ->
  case canRemoveValue(Name, Datetime, MeasurementType, Monitor) of
    true ->
      removeValueFromMonitor(Name, Datetime, MeasurementType, Monitor);
    false ->
      {error, "Value does not exist"}
  end;

removeValue({X, Y}, Datetime, MeasurementType, Monitor) ->
  case canRemoveValue({X, Y}, Datetime, MeasurementType, Monitor) of
    true ->
      Station = getStation({X, Y}, Monitor),
      removeValueFromMonitor(Station#station.name, Datetime, MeasurementType, Monitor);
    false ->
      {error, "Value does not exist"}
  end.


% funkcja pobierająca pomiar ze stacji na podstawie daty i typu pomiaru:

getOneValue(Name, Datetime, MeasurementType, Monitor) ->
  case stationExists(Name, Monitor) of
    true ->
      Station = getStation(Name, Monitor),

      case dict:find({Datetime, MeasurementType}, Station#station.measurements) of
        {ok, [{_, _, _, Value} | _]} -> Value;
        _ -> {error, "Value does not exist"}
      end;
    false ->
      {error, "Station with given name does not exist"}
  end.


% funkcja zwracająca średnią pomiarów danego typu z daej stacji:

getStationMean(Name, MeasurementType, Monitor) ->
  case stationExists(Name, Monitor) of
    true ->
      Station = getStation(Name, Monitor),

      Measurements = dict:filter(
        fun({D, M}, Value) -> M == MeasurementType end,
        Station#station.measurements
      ),

      Sum = dict:fold(
        fun(Key, [Value | _], AccIn) ->
          AccIn + Value#measurement.value end,
        0,
        Measurements
      ),

      All = dict:size(Measurements),

      case All of
        0 ->
          {error, "No measurements"};
        _ ->
          Sum / All
      end;
    false ->
      {error, "Station with given name does not exist"}
  end.


% funkcja zwracająca średnią pomiarów danego typu ze wszystkich stacji z danego dnia:

getDailyMean(MeasurementType, {{Year, Month, Day}, _}, Monitor) ->
  {Sum, Size} = dict:fold(
    fun(Key, [Station | _], {AllSum, AllSize}) ->
      StationMeasurements = dict:filter(
        fun({{{Y, Mon, D}, _}, M}, Value) ->
          (M == MeasurementType) and (Y == Year) and (Mon == Month) and (D == Day) end,
        Station#station.measurements
      ),

      StationSum = dict:fold(
        fun(Key, [Value | _], AccIn) ->
          AccIn + Value#measurement.value end,
        0,
        StationMeasurements
      ),

      {AllSum + StationSum, AllSize + dict:size(StationMeasurements)}
    end,
    {0, 0},
    Monitor#monitor.stations),

  case Size of
    0 ->
      {error, "No measurements"};
    _ ->
      Sum / Size
  end.


% pomocnicza funkcja zwracająca pomiary danego typu z danej stacji, które sa posortowane rosnąco:

getSortedMeasurements(Name, MeasurementType, Monitor) ->
  Station = getStation(Name, Monitor),

  lists:sort(
    fun(M1, M2) -> M1#measurement.value < M2#measurement.value end,
    lists:map(
      fun({Key, [Value | _]}) -> Value end,
      dict:to_list(
        dict:filter(
          fun({Datetime, M}, Value) -> M == MeasurementType end,
          Station#station.measurements
        )
      )
    )
  ).


% funkcja zwracająca najniższy pomiar danego typu z danej stacji:

getLowestValue(Name, MeasurementType, Monitor) ->
  case stationExists(Name, Monitor) of
    true ->
      case getSortedMeasurements(Name, MeasurementType, Monitor) of
        [] ->
          {error, "No values for given station and measurement type"};
        [{_, _, _, Lowest} | _] ->
          Lowest
      end;
    false ->
      {error, "Station with given name does not exist"}
  end.


% funkcja zwracająca ilość pomiarów danego typu o danym dniu z danej stacji:

getAmountOfValues(Name, MeasurementType, {{Year, Month, Day}, _}, Monitor) ->
  case stationExists(Name, Monitor) of
    true ->
      Station = getStation(Name, Monitor),
      dict:size(
        dict:filter(
          fun({{{Y, Mon, D}, _}, M}, Value) ->
            (M == MeasurementType) and (Y == Year) and (Mon == Month) and (D == Day) end,
          Station#station.measurements
        )
      );
    false ->
      {error, "Station with given name does not exist"}
  end.





