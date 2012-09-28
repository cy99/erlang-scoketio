%% Feel free to use, reuse and abuse the code in this file.

-module(websocket_handler).
-extends(xhr_polling).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3]).
-define(HEARBEAT_INTERVAL, socketio:get_env(heartbeat_interval)*1000).

init({_Any, http}, Req, []) ->
	case cowboy_http_req:header('Upgrade', Req) of
		{undefined, Req2} -> {ok, Req2, undefined};
		{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
		{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
	end.

handle(Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}], <<"503">>, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.

websocket_init(_Any, Req, []) ->
%% 	Req2 = cowboy_http_req:compact(Req),
	Session = get_session(Req),
	io:format("got Session is ~p~n", [Session]),
	case session_queue:lookup(Session) of
		undefined ->
			io:format("does not got room now~n"),
			void;
		Room ->
			Room ! {self(), subscribe, websocket}
	end,
	{ok, Req, undefined, hibernate}.

%% 处理来自客户端消息
websocket_handle({text, Data}, Req, State) ->
	Session = get_session(Req),
	Msg = binary_to_list(Data),
%% 	case Msg =:= <<"1::">> of
%% 		true ->
%% 			Room = session_queue:lookup(Session),
%% 			Room ! {self(), subscribe, websocket};
%% 		false ->
%% 			ok
%% 	end,

	io:format("websocket receive Msg is ~p and Session is ~p~n", [Msg, Session]),
	Result = string:join(["5:", "/chat", ?BASE_MODULE:do_post_msg({Session, Msg})], ":"),
	io:format("Result write back ~s~n", [Result]),
	BinaryResult = list_to_binary(Result),
	{reply, {text, BinaryResult}, Req, State, hibernate};
websocket_handle(_, Req, State) ->
	io:format("has nothing to do here~n", []),
	{ok, Req, State}.

%% 处理来做进程的消息的推送
websocket_info(first, Req, State) ->
	Session = get_session(Req),
	Room = session_queue:lookup(Session),
	timer:send_after(?HEARBEAT_INTERVAL, Room, {self(), post, "2::"}),
	%%{ok, Req, State, hibernate};
	io:format("now send back 1::~n"),
	{reply, {text, <<"1::">>}, Req, State, hibernate};
websocket_info(true, Req, State) ->
	io:format("invalude call with true parameter~n"),
	{ok, Req, State, hibernate};
websocket_info(Msg, Req, State) ->
	io:format("now send Msg ...............................................~n", []),
	io:format("now send Msg is ~s~n", [Msg]),
	{reply, {text, list_to_binary(Msg)}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

get_session(Req) ->
	{[_, _, _, Session], _} = cowboy_http_req:path(Req),
	binary_to_list(Session).