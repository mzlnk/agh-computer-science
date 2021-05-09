-module(erlchatter_gen_server).
-author("mzlnk").

-behavior(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([
  start_link/0,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-export([
  user_sign_in/3,
  user_sign_up/2,
  user_sign_out/1,
  test/1,
  message_all/2
]).

start_link() ->
  gen_server:start_link({local, erlchatter_gen_server}, ?MODULE, [], []).

init([]) ->
  {ok, erlchatter:initChat()}.

handle_call({user_sign_up, Login, Password}, _From, State) ->
  {UpdatedState, Reply} = erlchatter_provider:userSignUp(Login, Password, State),
  {reply, Reply, UpdatedState};

handle_call({user_sign_in, Login, Password, Pid}, _From, State) ->
  {UpdatedState, Reply} = erlchatter_provider:userSignIn(Login, Password, Pid, State),
  {reply, Reply, UpdatedState};

handle_call({user_sign_out, Token}, _From, State) ->
  {UpdatedState, Reply} = erlchatter_provider:userSignOut(Token, State),
  {reply, Reply, UpdatedState};

handle_call({list_online_users, Token}, _From, State) ->
  {UpdatedState, Reply} = erlchatter_provider:listOnlineUsers(Token, State),
  {reply, Reply, UpdatedState};

handle_call({test, Msg}, _From, State) ->
  {reply, "Hi!", State}.

handle_cast({message_all, Token, Message}, State) ->
  erlchatter_provider:messageAll(Token, Message, State),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  io:format("Server terminated. Reason: ~p~n", [Reason]),
  Reason.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% API
%%%===================================================================

user_sign_up(Login, Password) -> gen_server:call(?MODULE, {user_sign_up, Login, Password}).

user_sign_in(Login, Password, Pid) -> gen_server:call(?MODULE, {user_sign_in, Login, Password, Pid}).

user_sign_out(Token) -> gen_server:call(?MODULE, {user_sign_out, Token}).

test(Msg) -> gen_server:call(?MODULE, {test, Msg}).

message_all(Token, Message) -> gen_server:cast(?MODULE, {message_all, Token, Message}).
