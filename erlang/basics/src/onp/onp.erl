-module(onp).
-author("mzlnk").

%% API
-export([solve/1]).

solve(Operation) ->
  [R] = lists:foldl(fun solveOnp/2, [], lists:map(fun onpPrepare/1, string:tokens(Operation, " "))),
  R.

%% one-argument operators:
solveOnp("+", [A, B | T]) -> [(B + A) | T];
solveOnp("-", [A, B | T]) -> [(B - A) | T];
solveOnp("*", [A, B | T]) -> [(B * A) | T];
solveOnp("/", [A, B | T]) -> [(B / A) | T];
solveOnp("abs", [A | T]) -> [erlang:abs(A) | T];

%% two-arguments operators:
solveOnp("cos", [A | T]) -> [math:cos(A) | T];
solveOnp("sin", [A | T]) -> [math:sin(A) | T];
solveOnp("tan", [A | T]) -> [math:tan(A) | T];
solveOnp("sqrt", [A | T]) -> [math:sqrt(A) | T];
solveOnp("max", [A, B | T]) -> [erlang:max(A, B) | T];
solveOnp("min", [A, B | T]) -> [erlang:min(A, B) | T];

%% next numbers:
solveOnp(A, T) when is_float(A) or is_integer(A) -> [A |  T];

%% handle bad operators:
solveOnp(Operator, [A | T]) -> [A | T].

%% parse each string element to number (if value) or string (if operator):
onpPrepare(A) ->
  case string:to_float(A) of
    {error, _} -> case string:to_integer(A) of
                    {error, _} -> A;
                    {Integer, _} -> Integer
                  end;
    {Float, _} -> Float
  end.

