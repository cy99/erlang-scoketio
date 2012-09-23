-module(parent).
%% -compile().
-export([a/0, c/0]).
a() ->
	c().

c() ->
	io:format("~p parent is output now ~n", [?MODULE]).