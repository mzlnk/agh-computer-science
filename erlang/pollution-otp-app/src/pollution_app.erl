-module(pollution_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pollution_supervisor:start_link().

stop(_State) ->
    ok.
