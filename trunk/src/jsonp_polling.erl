%% @author yongboy <yong.boy@gmail.com>
%% @copyright 2012 yongboy <yong.boy@gmail.com>
%% @doc socketio.

-module(jsonp_polling).
-extends(xhr_polling).
-export([do_get/1, do_post/1, timeout_call/1]).

%%
%% API Functions
%%

%% @spec do_get({Session, Req}) -> void
%% @doc server for do get method
do_get({Session, Req}) ->
	Data = Req:parse_qs(),
	Msg = ?BASE_MODULE:do_get_msg({Session, Data}),
	Req:ok({"application/x-javascript; charset=utf-8", 
			[{"server", "socket.io server"}, {"X-XSS-Protection", "0"}, {"Connection", "keep-alive"}], 
			gen_output(proplists:get_value("i", Data), Msg)}).

%% @spec do_post(Any) -> void
%% @doc server for do post method
do_post({Session, Req}) ->
	Data = Req:parse_post(),
	OriMsg = proplists:get_value("d", Data),
	Msg2 = string:substr(OriMsg, 2, string:len(OriMsg)-2),
	Msg = re:replace(Msg2, "\\\\+", "", [global]),
	?BASE_MODULE:do_post_msg({Session, Msg}),
	Req:ok({"text/plain; charset=utf-8", [{"server", "socket.io server"}], "1"});
do_post(Any) ->
	?BASE_MODULE:do_post(Any).

%% @spec timeout_call(Any) -> void
%% @doc call xhr_polling::timeout_call(Any)
timeout_call(Any) ->
	?BASE_MODULE:time_call(Any).

%%
%% Local Functions
%%
gen_output(I, Msg) ->
	DescList = io_lib:format("io.j[~s]('~s');", [I, Msg]),
	lists:flatten(DescList).