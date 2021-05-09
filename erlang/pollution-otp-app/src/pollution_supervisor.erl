-module(pollution_supervisor).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
  supervisor:start_link({local, pollution_supervisor}, ?MODULE, []).

init([]) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 2,
    period => 5
  },

  ChildSpecs = [
    #{
      id => 'pollution_gen_server',
      start => {
        pollution_gen_server,
        start_link,
        []
      },
      restart => permanent,
      shutdown => 3000,
      type => worker,
      modules => [pollution_gen_server]
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
