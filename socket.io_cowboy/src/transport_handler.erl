-module(transport_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).
%% -define(TRANSPORTS, [{<<"xhr-polling">>, xhr_polling}, {<<"jsonp-polling">>, jsonp_polling}, {<<"htmlfile">>, htmlfile}]).
-define(TRANSPORTS, [{<<"jsonp-polling">>, jsonp_polling}, {<<"htmlfile">>, htmlfile}]).

init({_Any, http}, Req, []) ->
%% 	{ok, Req, undefined, ?TIMEOUT, hibernate}.
	{ok, Req, undefined}.

info(Message, Req, State) ->
    {loop, Req, State}.

handle(Req, State) ->
	{Method, _} = cowboy_http_req:method(Req),
	{Tokens, _} = cowboy_http_req:path(Req),
	case Method of
		'POST' ->
			%% OutputVal = list_to_binary(get_post_value(<<"d">>, Req)),
			%% lager:debug("POST METHOD NOW WITH outputVal ~p~n", [OutputVal]);
			%% 获取不带参数的post值
			%% OutputVal = cowboy_http_req:body_qs(Req),
			%% {ok, OutputVal, Req2} = cowboy_http_req:body(Req),
			%% lager:debug("POST METHOD NOW WITH outputVal ~p~n", [OutputVal]);
			{ok, Req3} = do_post(Tokens, Req);
		'GET' ->
			{T, Req2} = cowboy_http_req:qs_val(<<"t">>, Req),
			lager:debug("**************************Time is ~p~n", [T]),
			{ok, Req22} = cowboy_http_req:set_resp_header(<<"Connection">>, <<"keep-alive">>, Req2),
%% 			{ok, Req3} = do_request(Tokens, cowboy_http_req:compact(Req22));
			{ok, Req3} = do_request(Tokens, Req22);
		_ ->
			Req3 = Req,
			lager:debug("not allowed request here ... with Req ~p~n", [Req]),
			OutputVal = <<"not allowed request">>
	end,
%% 	Headers = [{<<"Content-Type">>, <<"text/html, charset=utf-8">>}],
%% 	{ok, Req3} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/html, charset=utf-8">>}], OutputVal, Req2),
	{ok, Req3, State}.

%%
%% Local Functions
%%
get_transport(Transport) ->
	proplists:get_value(Transport, ?TRANSPORTS).

do_post([<<"socket.io">>, <<"1">>, Transport, Session], Req) ->
	NewTransport = get_transport(Transport),
	NewTransport:do_post({binary_to_list(Session), Req});
do_post([], Req) ->
	cowboy_http_req:reply(404, Req);
do_post(_, Req) ->
	cowboy_http_req:reply(500, Req).

do_request([<<"socket.io">>, <<"1">>], Req) ->
	UUID = uuid_server:gen(),
	Room = session_queue:register(UUID),
	xhr_polling:set_timeout(Room, UUID),
	Msg = io_lib:format("~s:~p:~p:~s", [UUID, socketio:get_env(heartbeat_timeout), socketio:get_env(close_timeout), socketio:get_env(allow_transports)]),
	OutputVal = list_to_binary(Msg),
	cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain, charset=utf-8">>}], OutputVal, Req);

do_request([<<"socket.io">>, <<"1">>, Transport, Session], Req) ->
	NewTransport = get_transport(Transport),
	NewTransport:do_get({binary_to_list(Session), Req});
do_request(_, Req) ->
	cowboy_http_req:reply(404, Req).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

terminate(_Req, _State) ->
	ok.
