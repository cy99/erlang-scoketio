%% @author yongboy <yong.boy@gmail.com>
%% @copyright 2012 yongboy <yong.boy@gmail.com>
%% @doc socketio.

-module(websocket_handler).
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
	case session_queue:lookup(Session) of
		undefined ->
			lager:debug("does not got room now"),
			void;
		Room ->
			Room ! {self(), subscribe, websocket}
	end,
	{ok, Req, undefined, hibernate}.

%% handle message from client
websocket_handle({text, Data}, Req, State) ->
	Session = get_session(Req),
	Msg = binary_to_list(Data),

	HandleMsg = common_polling:do_post_msg({Session, Msg}),
	Room = session_queue:register(Session),
	Room ! {self(), getEndpoint},
	Endpoint = receive
		{endpoint, TargetEndpoint} ->
			TargetEndpoint
	end,
	Result = string:join(["5:", Endpoint, HandleMsg], ":"),
	BinaryResult = list_to_binary(Result),
	{reply, {text, BinaryResult}, Req, State, hibernate};
websocket_handle(_, Req, State) ->
	lager:debug("has nothing to do here", []),
	{ok, Req, State}.

%% handle message from process
websocket_info({reply, first}, Req, State) ->
	Session = get_session(Req),
	Room = session_queue:lookup(Session),
	timer:send_after(?HEARBEAT_INTERVAL, Room, {self(), post, "2::"}),
	{reply, {text, <<"1::">>}, Req, State, hibernate};
websocket_info({reply, Msg}, Req, State) ->
	{reply, {text, list_to_binary(Msg)}, Req, State, hibernate};
websocket_info(Any, Req, State) ->
	lager:debug("~p invalude call with true parameter", [Any]),
	{ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

get_session(Req) ->
	{[_, _, _, Session], _} = cowboy_http_req:path(Req),
	binary_to_list(Session).