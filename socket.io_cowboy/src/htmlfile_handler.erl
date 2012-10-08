-module(htmlfile_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).
-define(HEARBEAT_TIMEOUT, socketio:get_env(heartbeat_timeout)*1000).

init({_Transport, http}, Req, State) ->
	{ok, Req, State}.

%% POST/Short Request
handle(Req, State) ->
	{Method, Req1} = cowboy_http_req:method(Req),
	{[_, _, _, BinarySession], Req2} = cowboy_http_req:path(Req1),
	Session = binary_to_list(BinarySession),
	case Method of
		'POST' ->
			handle_post(Req2, Session, State);
		'GET' ->
			handle_get(Req2, Session, State)
	end.

handle_get(Req, Session, _State) ->
	Room = session_queue:lookup(Session),
	common_polling:set_timeout(Room, Session, ?HEARBEAT_TIMEOUT),

	{ok, Reply} = cowboy_http_req:chunked_reply(200, [{<<"Content-Type">>, <<"text/html; charset=utf-8">>}], Req),
	cowboy_http_req:chunk("<html><body><script>var _ = function (msg) { parent.s._(msg, document); };</script>                                                                                                                                                                                                                  ", 
								Reply),
	Room ! {self(), subscribe, htmlfile},
	wait_data(Session, Room, Reply),
	Room ! {self(), end_connect},
	{ok, Req, undefined_state}.

wait_data(Session, Room, Reply) ->
    receive
        {reply, first} ->
			cowboy_http_req:chunk(gen_output("1::"), Reply);
		{reply, Message} ->
			cowboy_http_req:chunk(gen_output(Message), Reply);
		_Any ->
			void
    end,
	
    wait_data(Session, Room, Reply).

gen_output(String) ->
	DescList = io_lib:format("<script>_('~s');</script>", [String]),
	list_to_binary(DescList).

handle_post(Req, Session, _State) ->
	Result = case cowboy_http_req:body(Req) of
		{ok, Data, Req2} ->
			Msg = binary_to_list(Data),
			common_polling:do_post_msg({Session, Msg});
		{error, timeout} ->
			Req2 = Req,
			"1"
	end,
	{_, Req3} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain; charset=utf-8">>}], list_to_binary(Result), Req2),
	{ok, Req3, Session}.

terminate(_Req, _State) ->
	ok.