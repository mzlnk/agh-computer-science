-module(pollution_test).
-author("mzlnk").

-include_lib("eunit/include/eunit.hrl").

addStation_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation("Aleja 1", {50, 50}, P1),

  ?assertMatch({error, _}, pollution:addStation("Aleja 2", {50, 50}, P2)),
  ?assertMatch({error, _}, pollution:addStation("Aleja 1", {100, 100}, P2)).

addValue_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation("Aleja 1", {50, 50}, P1),

  PointInTime = calendar:local_time(),
  P3 = pollution:addValue("Aleja 1", PointInTime, temperature, 24, P2),

  ?assertMatch({error, _}, pollution:addValue("Aleja 25", calendar:local_time(), temperature, 24, P3)),
  ?assertMatch({error, _}, pollution:addValue("Aleja 1", PointInTime, temperature, 30, P3)).

removeValue_test() ->
  P1 = pollution:createMonitor(),
  ?assertMatch({error, _}, pollution:removeValue("Aleja 1", calendar:local_time(), temperature, P1)).

getOneValue_test() ->
  P1 = pollution:createMonitor(),

  Time = calendar:local_time(),
  P2 = pollution:addStation("Aleja 1", {50, 50}, P1),
  P3 = pollution:addValue("Aleja 1", Time, temperature, 30, P2),

  ?assertEqual(30, pollution:getOneValue("Aleja 1", Time, temperature, P3)),
  ?assertMatch({error, _}, pollution:getOneValue("Aleja 2", Time, temperature, P3)).

getStationMean_test() ->
  P1 = pollution:createMonitor(),

  P2 = pollution:addStation("Aleja 1", {50, 50}, P1),
  P3 = pollution:addValue("Aleja 1", {{2020, 01, 01}, {12, 12, 12}}, temperature, 30, P2),
  P4 = pollution:addValue("Aleja 1", {{2020, 01, 02}, {12, 12, 12}}, temperature, 10, P3),

  ?assertEqual(20.0, pollution:getStationMean("Aleja 1", temperature, P4)),
  ?assertMatch({error, _}, pollution:getStationMean("Aleja 1", pm25, P4)),
  ?assertMatch({error, _}, pollution:getStationMean("Aleja 25", temperature, P4)).


getLowestValue_test() ->
  P1 = pollution:createMonitor(),

  P2 = pollution:addStation("Aleja 1", {50, 50}, P1),
  P3 = pollution:addValue("Aleja 1", {{2020, 01, 01}, {12, 12, 12}}, temperature, 30, P2),
  P4 = pollution:addValue("Aleja 1", {{2020, 01, 02}, {12, 12, 12}}, temperature, 10, P3),

  ?assertEqual(10, pollution:getLowestValue("Aleja 1", temperature, P4)).

getDailyMean_test() ->
  P1 = pollution:createMonitor(),

  P2 = pollution:addStation("Aleja 1", {50, 50}, P1),
  P3 = pollution:addValue("Aleja 1", {{2020, 01, 01}, {13, 12, 12}}, temperature, 30, P2),
  P4 = pollution:addValue("Aleja 1", {{2020, 01, 01}, {12, 12, 12}}, temperature, 10, P3),

  ?assertEqual(20.0, pollution:getDailyMean(temperature, {{2020, 01, 01}, {}}, P4)).

getAmountOfValues_test() ->
  P1 = pollution:createMonitor(),

  P2 = pollution:addStation("Aleja 1", {50, 50}, P1),
  P3 = pollution:addValue("Aleja 1", {{2020, 01, 01}, {13, 12, 12}}, temperature, 30, P2),
  P4 = pollution:addValue("Aleja 1", {{2020, 01, 01}, {14, 12, 12}}, temperature, 10, P3),
  P5 = pollution:addValue("Aleja 1", {{2020, 01, 01}, {15, 12, 12}}, temperature, 10, P4),
  P6 = pollution:addValue("Aleja 1", {{2020, 01, 02}, {13, 12, 12}}, temperature, 10, P5),


  ?assertEqual(3, pollution:getAmountOfValues("Aleja 1", temperature, {{2020, 01, 01}, {}}, P6)),
  ?assertMatch({error, _}, pollution:getAmountOfValues("Aleja 2", temperature, {{2020, 01, 01}, {}}, P6)).