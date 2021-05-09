-module(erlchatter_supervisor).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, erlchatter_supervisor}, ?MODULE, []).

init([]) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 2,
    period => 5
  },

  ChildSpecs = [
    #{
      id => 'erlchatter_gen_server',
      start => {
        erlchatter_gen_server,
        start_link,
        []
      },
      restart => permanent,
      shutdown => 3000,
      type => worker,
      modules => [erlchatter_gen_server]
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
