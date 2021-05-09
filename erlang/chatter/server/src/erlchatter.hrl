-author("mzlnk").

-type userStatus() :: online | offline.
-type userRole() :: user | moderator | admin.

-record(token, {login :: string(), token :: string()}).
-record(user, {login :: string(), pid, status = offline :: userStatus(), roles = [user] :: [userRole()]}).
-record(chat, {users = dict:new(), tokens = dict:new(), passwords = dict:new()}).
