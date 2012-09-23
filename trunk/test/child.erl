-module(child).
-compile(export_all).
-extends(parent).

c() ->
	io:format("~p child is output now ~n", [?MODULE]).