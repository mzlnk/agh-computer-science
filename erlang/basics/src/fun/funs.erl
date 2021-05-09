-module(funs).
-author("mzlnk").

%% API
-export([map/2, filter/2, digitSum/1]).

map(Mapper, List) -> [Mapper(X) || X <- List].

filter(Filter, List) -> [X || X <- List, Filter(X)].

digitSum(Number) -> lists:foldl(fun(X, Acc) -> Acc + X end, 0, [list_to_integer([C]) || C <- integer_to_list(Number)]).