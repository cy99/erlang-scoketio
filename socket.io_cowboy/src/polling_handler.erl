%% Feel free to use, reuse and abuse the code in this file.

-module(polling_handler).
-extends(xhr_polling).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, info/3, terminate/2]).
-define(TRANSPORTS, [{<<"xhr-polling">>, xhr_polling}, {<<"jsonp-polling">>, jsonp_polling}, {<<"htmlfile">>, htmlfile}]).
-define(HEARBEAT_INTERVAL, socketio:get_env(heartbeat_interval)*1000).
-define(HEARBEAT_TIMEOUT, socketio:get_env(heartbeat_timeout)*1000).

init({_Transport, http}, Req, State) ->
	lager:debug("polling_handler was called now~"),
	{Method, Req1} = cowboy_http_req:method(Req),
	{[_, _, _, BinarySession], Req2} = cowboy_http_req:path(Req1),
	Session = binary_to_list(BinarySession),
	case Method of
		'POST' ->
			lager:debug("Now is Post Method ~"),
			{loop, Req2, Session};
		<<"POST">> ->
			lager:debug("Now is Post Method2 ~"),
			{loop, Req2, Session};
		'GET' ->
			Room = session_queue:lookup(Session),
			?BASE_MODULE:set_timeout(Room, Session, ?HEARBEAT_TIMEOUT),
			lager:debug("wait mssage now ...~n"),
			Room ! {self(), subscribe, xhr_polling},
			erlang:send_after(?HEARBEAT_INTERVAL, self(), timeout),
			{loop, Req2, {Room, Session}, ?HEARBEAT_INTERVAL, hibernate}
	end.

%% POST/Short Request
handle(Req, State) ->
	lager:debug("DO POST Handle here ..."),
	Session = State,
	Result = case cowboy_http_req:body(Req) of
		{ok, Data, Req2} ->
			Msg = binary_to_list(Data),
			lager:debug("do_post got Msg is ~p~n", [Msg]),
			?BASE_MODULE:do_post_msg({Session, Msg});
		{error,timeout} ->
			Req2 = Req,
			io : format("got timeout now ~"),
			"1"
	end,
	lager:debug("got Result is ~p~n", [Result]),
	{_, Req3} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain; charset=utf-8">>}], list_to_binary(Result), Req2),
	{ok, Req3, State}.

%% LONG POLLING
info(first, Req, State) ->
	lager:debug("Got State is ~p", [State]),
    output("1::", Req, State);
info(timeout, Req, State) ->
	lager:debug("Got State is ~p", [State]),
	output("8::", Req, State);
info(true, Req, State) ->
	lager:debug("Got State is ~p", [State]),
	lager:error("GOT ERROR MESSAGE IS TRUE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"),
    {loop, Req, State, hibernate};
info(Message, Req, State) ->
     output(Message, Req, State).

output(Message, Req, State = {Room, Session}) ->
	lager:debug("has got Message ~p~n", [Message]),
	Room ! {self(), end_connect},
	{ok, Req2} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain; charset=utf-8">>}], list_to_binary(Message), Req),
	{ok, Req2, undefined_state}.

terminate(_Req, _State) ->
	ok.