-module(erlchatter).
-author("mzlnk").

-include("erlchatter.hrl").

%% API
-export([
  initChat/0,
  userSignUp/3,
  userSignIn/4,
  userSignOut/2,
  listOnlineUsers/1,
  hasUserRole/3,
  getUserLoginFromToken/2,
  validateToken/2
]).

initChat() -> #chat{}.

% generate new token:

generateToken() -> base64:encode_to_string(crypto:strong_rand_bytes(32)).

% check methods:

loginExists(Login, Chat) ->
  dict:is_key(Login, Chat#chat.users).

isUserOnline(Login, Chat) ->
  case dict:find(Login, Chat#chat.users) of
    error -> {error, "User with given login does not exist"};
    {ok, User} ->
      User#user.status == online
  end.

isUserCredentialsCorrect(Login, Password, Chat) ->
  case dict:find(Login, Chat#chat.passwords) of
    error -> false;
    {ok, StoredPassword} ->
      EncryptedPassword = crypto:hash(sha512, Password),
      StoredPassword == EncryptedPassword
  end.

% retrieve user login from token:

getUserLoginFromToken(Token, Chat) -> {
  case dict:find(Token, Chat#chat.tokens) of
    error -> {error, "Invalid token"};
    {ok, TokenDetails} -> TokenDetails#token.login
  end
}.

% validate token:

validateToken(Token, Chat) ->
  case dict:is_key(Token, Chat#chat.tokens) of
    true -> valid;
    false -> invalid
  end.

% user sign up:

userSignUpToChat(Login, Password, Chat) ->
  Chat#chat{
    users = dict:store(
      Login,
      #user{
        login = Login
      },
      Chat#chat.users
    ),
    passwords = dict:store(
      Login,
      crypto:hash(sha512, Password),
      Chat#chat.passwords
    )
  }.

userSignUp(Login, Password, Chat) ->
  case loginExists(Login, Chat) of
    true ->
      {error, "User with given login already exists"};
    false ->
      userSignUpToChat(Login, Password, Chat)
  end.

% user sign in:

userSignInToChat(Login, Pid, Chat) ->
  Token = generateToken(),

  {
    Token,
    Chat#chat{
      tokens = dict:store(
        Token,
        #token{
          login = Login,
          token = Token
        },
        Chat#chat.tokens
      ),
      users = dict:update(
        Login,
        fun(User) ->
          User#user{
            status = online,
            pid = Pid
          }
        end,
        Chat#chat.users
      )
    }
  }.

userSignIn(Login, Password, Pid, Chat) ->
  case isUserCredentialsCorrect(Login, Password, Chat) of
    false -> {error, "Bad credentials"};
    true ->
      case isUserOnline(Login, Chat) of
        error -> {error, "Bad credentials"};
        true -> {error, "User already signed in"};
        false ->
          userSignInToChat(Login, Pid, Chat)
      end
  end.

% User sign out:

userSignOutFromChat(Login, Chat) ->
  Chat#chat{
    users = dict:update(
      Login,
      fun(User) ->
        User#user{
          status = offline,
          pid = none
        }
      end,
      Chat#chat.users
    )
  }.

userSignOut(Login, Chat) ->
  {LoginName} = Login,
  case loginExists(LoginName, Chat) of
    false -> {error, "User with given login does not exist"};
    true -> userSignOutFromChat(LoginName, Chat)
  end.

% list online users:

listOnlineUsers(Chat) ->
  lists:filter(
    fun(User) -> User#user.status == online end,
    lists:map(
      fun({Key, Value}) -> Value end,
      dict:to_list(Chat#chat.users)
    )
  ).

% user has role:

hasUserRole(Login, Role, Chat) ->
  case loginExists(Login, Chat) of
    false -> {error, "User with given login does not exist"};
    true ->
      User = dict:find(Login, Chat),
      lists:any(
        fun(R) -> R == Role end,
        User#user.roles
      )
  end.
