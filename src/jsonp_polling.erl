-module(jsonp_polling).
-compile(export_all).
-extends(xhr_polling).

%% -export([do_get/1, do_post/1, timeout_call/1]).

%%
%% API Functions
%%
do_get({Session, Req}) ->
	Data = Req:parse_qs(),
	Msg = ?BASE_MODULE:do_get_msg({Session, Data}),
	Req:ok({"text/javascript; charset=UTF-8", [{"server", "Mochiweb-Test"}], gen_output(format(proplists:get_value("i", Data), Msg))}).

do_post({Session, Req}) ->
	Data = Req:parse_post(),
	OriMsg = proplists:get_value("d", Data),
	Msg2 = string:substr(OriMsg, 2, string:len(OriMsg)-2),
	Msg = re:replace(Msg2, "\\\\+", "", [global]),
	io:format("i-Msg is ~s~n", [Msg]),
	?BASE_MODULE:do_post_msg({Session, Msg}),	
	Req:ok({"text/javascript; charset=UTF-8", [{"server", "Mochiweb-Test"}], gen_output(format(proplists:get_value("i", Req:parse_qs()), "1"))});
do_post(_) ->
	io:format("missing any thing at all now~n").

format(I, Msg) ->
	lists:flatten(io_lib:format("io.j[~s](\"~s\");", [I, Msg])).

gen_output(String) ->
%% 	io:format("now String is ~s ~n", [String]),
%% 	[DescList] = io_lib:format("~ts", [String]),
%% 	[DescList] = io_lib:format("~ts", [String]),
	DescList = String,
    Bin = erlang:iolist_to_binary(DescList).