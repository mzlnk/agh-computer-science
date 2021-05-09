-module(erlchatter_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlchatter_supervisor:start_link().

stop(_State) ->
    ok.

%% internal functions
