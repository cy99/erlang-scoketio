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
	Req:ok({"application/x-javascript; charset=utf-8", [{"server", "Mochiweb-Test"}, {"Connection", "keep-alive"}], gen_output(format(proplists:get_value("i", Data), Msg))}).

do_post({Session, Req}) ->
	Data = Req:parse_post(),
	OriMsg = proplists:get_value("d", Data),
	Msg2 = string:substr(OriMsg, 2, string:len(OriMsg)-2),
	Msg = re:replace(Msg2, "\\\\+", "", [global]),
	io:format("i-Msg is ~s~n", [Msg]),
	%% TODO 需要处理IE下乱码问题
	?BASE_MODULE:do_post_msg({Session, Msg}),	
	Req:ok({"text/plain; charset=utf-8", [{"server", "Mochiweb-Test"}], "1"});
do_post(_) ->
	io:format("missing any thing at all now~n").

format(I, Msg) ->
	lists:flatten(io_lib:format("io.j[~s]('~s');", [I, Msg])).

gen_output(String) ->
	[DescList] = io_lib:format("~ts", [String]),
    Bin = erlang:iolist_to_binary(DescList).