%% Feel free to use, reuse and abuse the code in this file.

-module(polling_handler).
-extends(xhr_polling).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, info/3, terminate/2]).
-define(TRANSPORTS, [{<<"xhr-polling">>, xhr_polling}, {<<"jsonp-polling">>, jsonp_polling}, {<<"htmlfile">>, htmlfile}]).
-define(HEARBEAT_INTERVAL, socketio:get_env(heartbeat_interval)*1000).
-define(HEARBEAT_TIMEOUT, socketio:get_env(heartbeat_timeout)*1000).

init({_Transport, http}, Req, _State) ->
	{Method, Req1} = cowboy_http_req:method(Req),
	{[_, _, _, BinarySession], Req2} = cowboy_http_req:path(Req1),
	Session = binary_to_list(BinarySession),
	case Method of
		'POST' ->
			{ok, Req2, Session};
		'GET' ->
			Room = session_queue:lookup(Session),
			?BASE_MODULE:set_timeout(Room, Session, ?HEARBEAT_TIMEOUT),
			Room ! {self(), subscribe, xhr_polling},
			TimeoutRef = erlang:send_after(?HEARBEAT_INTERVAL, self(), timeout),
			{loop, Req2, {Room, Session, TimeoutRef}, ?HEARBEAT_INTERVAL + 1000, hibernate}
	end.

%% POST/Short Request
handle(Req, State) ->
	Session = State,
	Result = case cowboy_http_req:body(Req) of
		{ok, Data, Req2} ->
			Msg = binary_to_list(Data),
			?BASE_MODULE:do_post_msg({Session, Msg});
		{error, timeout} ->
			Req2 = Req,
			"1"
	end,
	{_, Req3} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain; charset=utf-8">>}], list_to_binary(Result), Req2),
	{ok, Req3, State}.

%% LONG POLLING
info({reply, first}, Req, State) ->
    output("1::", Req, State);
info(timeout, Req, State) ->
	output("8::", Req, State);
info({reply, Message}, Req, State) ->
     output(Message, Req, State);
info(Any, Req, State) ->
	lager:debug("got unwanted message is ~p~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", [Any]),
    {loop, Req, State, hibernate}.

output(Message, Req, _State = {Room, _Session, TimeoutRef}) ->
	Room ! {self(), end_connect},
	erlang:cancel_timer(TimeoutRef),
	{ok, Req2} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain; charset=utf-8">>}], list_to_binary(Message), Req),
	{ok, Req2, undefined_state}.

terminate(_Req, _State) ->
	ok.