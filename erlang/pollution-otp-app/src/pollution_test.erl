-module(pollution_test).
-author("mzlnk").

-include_lib("eunit/include/eunit.hrl").

getOneValue_test() ->
  P1 = pollution:createMonitor(),

  Time = calendar:local_time(),
  P2 = pollution:addStation("Aleja 1", {50, 50}, P1),
  P3 = pollution:addValue("Aleja 1", Time, temperature, 30, P2),

  ?assertEqual(30, pollution:getOneValue("Aleja 1", Time, temperature, P3)),
  ?assertMatch({error, _}, pollution:getOneValue("Aleja 2", Time, temperature, P3)).