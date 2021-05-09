-module(pollution_gen_server_test).
-author("mzlnk").

-include_lib("eunit/include/eunit.hrl").


% test if system rebuilds after crash:

crash_test() ->
  ?assertEqual(ok, pollution_gen_server:addStation("A", {100, 100})),
  pollution_gen_server:crash(),
  ?assertEqual(ok, pollution_gen_server:addStation("A", {100, 100})).


% test if system's state is not lost after user type error:

userError_test() ->
  pollution_gen_server:addStation("B", {200, 200}),
  pollution_gen_server:addValue("B", {{2020,1,1},{12,0,0}}, temperature, 30),
  pollution_gen_server:addValue("B", {{2020,1,1},{13,0,0}}, temperature, 40),

  pollution_gen_server:addStation("C", {200, 200}),
  ?assertEqual(35.0, pollution_gen_server:getStationMean("B", temperature)).



% test if system's state is lost after crash:

stateLoss_test() ->
  pollution_gen_server:addStation("D", {300, 300}),
  pollution_gen_server:addValue("D", {{2020,1,1},{12,0,0}}, temperature, 30),
  pollution_gen_server:addValue("D", {{2020,1,1},{13,0,0}}, temperature, 40),

  ?assertEqual(30, pollution_gen_server:getOneValue("D", {{2020,1,1},{12,0,0}}, temperature)),

  pollution_gen_server:crash(),

  ?assertMatch({error, _}, pollution_gen_server:getStationMean("D", temperature)).

