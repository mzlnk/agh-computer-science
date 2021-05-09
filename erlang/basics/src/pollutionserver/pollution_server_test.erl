-module(pollution_server_test).
-author("mzlnk").

-include_lib("eunit/include/eunit.hrl").

addStation_test() ->
  pollution_server:start(),

  pollution_server:addStation("Aleja 1", {50, 50}),

  ?assertMatch({error, _}, pollution_server:addStation("Aleja 2", {50, 50})),
  ?assertMatch({error, _}, pollution_server:addStation("Aleja 1", {100, 100})),

  pollution_server:stop().

addValue_test() ->
  pollution_server:start(),

  pollution_server:addStation("Aleja 1", {50, 50}),

  PointInTime = calendar:local_time(),
  pollution_server:addValue("Aleja 1", PointInTime, temperature, 24),

  ?assertMatch({error, _}, pollution_server:addValue("Aleja 25", calendar:local_time(), temperature, 24)),
  ?assertMatch({error, _}, pollution_server:addValue("Aleja 1", PointInTime, temperature, 30)),

  pollution_server:stop().

removeValue_test() ->
  pollution_server:start(),

  ?assertMatch({error, _}, pollution_server:removeValue("Aleja 1", calendar:local_time(), temperature)),

  pollution_server:stop().

getOneValue_test() ->
  pollution_server:start(),

  Time = calendar:local_time(),
  pollution_server:addStation("Aleja 1", {50, 50}),
  pollution_server:addValue("Aleja 1", Time, temperature, 30),

  ?assertEqual(30, pollution_server:getOneValue("Aleja 1", Time, temperature)),
  ?assertMatch({error, _}, pollution_server:getOneValue("Aleja 2", Time, temperature)),

  pollution_server:stop().

getStationMean_test() ->
  pollution_server:start(),

  pollution_server:addStation("Aleja 1", {50, 50}),
  pollution_server:addValue("Aleja 1", {{2020, 01, 01}, {12, 12, 12}}, temperature, 30),
  pollution_server:addValue("Aleja 1", {{2020, 01, 02}, {12, 12, 12}}, temperature, 10),

  ?assertEqual(20.0, pollution_server:getStationMean("Aleja 1", temperature)),
  ?assertMatch({error, _}, pollution_server:getStationMean("Aleja 1", pm25)),
  ?assertMatch({error, _}, pollution_server:getStationMean("Aleja 25", temperature)),

  pollution_server:stop().


getLowestValue_test() ->
  pollution_server:start(),

  pollution_server:addStation("Aleja 1", {50, 50}),
  pollution_server:addValue("Aleja 1", {{2020, 01, 01}, {12, 12, 12}}, temperature, 30),
  pollution_server:addValue("Aleja 1", {{2020, 01, 02}, {12, 12, 12}}, temperature, 10),

  ?assertEqual(10, pollution_server:getLowestValue("Aleja 1", temperature)),

  pollution_server:stop().

getDailyMean_test() ->
  pollution_server:start(),

  pollution_server:addStation("Aleja 1", {50, 50}),
  pollution_server:addValue("Aleja 1", {{2020, 01, 01}, {13, 12, 12}}, temperature, 30),
  pollution_server:addValue("Aleja 1", {{2020, 01, 01}, {12, 12, 12}}, temperature, 10),

  ?assertEqual(20.0, pollution_server:getDailyMean(temperature, {{2020, 01, 01}, {}})),

  pollution_server:stop().

getAmountOfValues_test() ->
  pollution_server:start(),

  pollution_server:addStation("Aleja 1", {50, 50}),
  pollution_server:addValue("Aleja 1", {{2020, 01, 01}, {13, 12, 12}}, temperature, 30),
  pollution_server:addValue("Aleja 1", {{2020, 01, 01}, {14, 12, 12}}, temperature, 10),
  pollution_server:addValue("Aleja 1", {{2020, 01, 01}, {15, 12, 12}}, temperature, 10),
  pollution_server:addValue("Aleja 1", {{2020, 01, 02}, {13, 12, 12}}, temperature, 10),


  ?assertEqual(3, pollution_server:getAmountOfValues("Aleja 1", temperature, {{2020, 01, 01}, {}})),
  ?assertMatch({error, _}, pollution_server:getAmountOfValues("Aleja 2", temperature, {{2020, 01, 01}, {}})),

  pollution_server:stop().