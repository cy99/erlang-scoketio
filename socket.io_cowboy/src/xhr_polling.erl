%% @author yongboy <yong.boy@gmail.com>
%% @copyright 2012 yongboy <yong.boy@gmail.com>
%% @doc socketio.

-module(xhr_polling).
-export([do_get/1, do_post/1, timeout_call/1, set_timeout/2, set_timeout/3, do_get_msg/1, do_post_msg/1]).
-define(HEARBEAT_INTERVAL, socketio:get_env(heartbeat_interval)*1000).
-define(HEARBEAT_TIMEOUT, socketio:get_env(heartbeat_timeout)*1000).

%%
%% API Functions
%%
%% @spec do_get({Session, Req}) -> void
%% @doc server for do get method
do_get({Session, Req}) ->
	io:format("session is ~s~n", [Session]),
	Disconnected = case cowboy_http_req:qs_val(<<"disconnect">>, Req) of
		{undefined, NewReg} -> false;
		{_, NewReg} -> true
	end,
	Msg = do_get_msg({Session, Disconnected}),
	io:format("had got the msg now render message ~s~n", [Msg]),
	cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain, charset=utf-8">>}], list_to_binary(Msg), NewReg).

%% @spec do_get_msg({Session, Data}) -> Msg
%% @doc just export for htmlfile/jsonp module to call
do_get_msg({Session, Disconnected}) ->
	io:format("going to got message now ~n"),
	Room = session_queue:register(Session),
	case Room of
		undefined ->
			"7:::[\"Request Invalide\"]+[\"Please do not do that!\"]";
		_ ->
			io:format("going to 2 got message now ~n"),
			do_handle_get_msg({Session, Disconnected}, Room)
	end.

%% @spec do_post(Any) -> void
%% @doc server for do post method
do_post({Session, Req}) ->
	{ok, Data, _} = cowboy_http_req:body(Req),
	Msg = binary_to_list(Data),
	io:format("got Msg is ~p~n", [Msg]),
	Result = do_post_msg({Session, Msg}),
	io:format("got Result is ~p~n", [Result]),
	
	cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain, charset=utf-8">>}], list_to_binary(Result), Req);
do_post(_) ->
	io:format("missing any thing at all now~n").

%% @spec do_post_msg({Session,Msg}) -> Msg
%% @doc just export for htmlfile/jsonp module to call
do_post_msg({Session,Msg}) ->
	{[Type, MessageId, Endpoint, SubMsgData]} = socketio_decode:decode(Msg),
	Room = session_queue:lookup(Session),
	case Room of
		undefined ->
			"7::" ++ Endpoint ++ ":[\"Request Invalide\"]+[\"Please do not do that!\"]";
		_ ->
			do_handle_post_msg({Type, MessageId, Endpoint, SubMsgData}, {Session,Msg}, Room),
			"1"
	end.

%% @spec timeout_call(Any) -> void
%% @doc timeout call
timeout_call({Room, Session}) ->
	Room ! {self(), unsubscribe, Session};
timeout_call({Room, Session, Endpoint, Type}) ->
	Implement = map_server:lookup(Endpoint),
	%% 	Room = session_queue:lookup(Session),
	Implement:on_disconnect({Session, Endpoint, timeout}, fun(SendMsg, Others) ->
				send_call({Session, Type, Endpoint}, SendMsg, Others)
	end),
	Room ! {self(), unsubscribe, Session}.

%% @spec set_timeout(Room, Session) -> void
%% @doc set timer execute one time with default ?HEARBEAT_TIMEOUT
set_timeout(Room, Session) ->
	set_timeout(Room, Session, ?HEARBEAT_TIMEOUT).

%% @spec set_timeout(Room, Session, Timeout) -> void
%% @doc set timer execute one time
set_timeout(Room, Session, Timeout) ->
	Room ! {self(),getEndpoint},
	Endpoint = receive
		Any ->
			Any
	end,
	Args = case Endpoint of
		undefined ->
			{Room, Session};
		_ ->
			{Room, Session, Endpoint, "5"}
	end,
	TimeRef = case timer:apply_after(Timeout, ?MODULE, timeout_call, [Args]) of
		{ok, TRef} ->
			TRef;
		{error, Reason} ->
			undefined
	end,
	Room ! {self(), timeout, TimeRef}.

%%
%% Local Functions
%%
do_handle_post_msg({Type, MessageId, Endpoint, SubMsgData}, {Session,Msg}, Room) ->
	Implement = map_server:lookup(Endpoint),
	case Type of
		"0" ->
			Implement:on_disconnect({Session, Endpoint, SubMsgData}, fun(SendMsg, Others) ->
				send_call({Session, Type, Endpoint}, SendMsg, Others)
			end),
			Room ! {self(), unsubscribe, Session};
		"1" ->
			Room ! {self(), endpoint, Endpoint},
			io:format("post Msg ~s~n", [Msg]),
			Room ! {self(), post, Msg},
			Implement:on_connect({Session, MessageId, Endpoint, SubMsgData}, fun(SendMsg, Others) ->
				send_call({Session, Type, Endpoint}, SendMsg, Others)
			end);
		"2" ->
			set_timeout(Room, Session, ?HEARBEAT_TIMEOUT),
			timer:send_after(?HEARBEAT_INTERVAL, Room, {self(), post, "2::"});
		"5" ->
			Implement:on_message({Session, Type, MessageId, Endpoint, SubMsgData}, fun(SendMsg, Others) ->
				send_call({Session, Type, Endpoint}, SendMsg, Others)
			end)
	end.

do_handle_get_msg({Session, Disconnected}, Room) ->
	case Disconnected of
		true ->
			set_timeout(Room, Session,1),
			"";
		false ->
			set_timeout(Room, Session, ?HEARBEAT_TIMEOUT),
			io:format("wait mssage now ...~n"),
			Room ! {self(), subscribe, ?MODULE},
			Msg = receive
					first ->
						"1::";
					Message ->
						Message
				after ?HEARBEAT_INTERVAL ->
						"8::"
				end,
			io:format("has got MSG ~p~n", [Msg]),
			Room ! {self(), end_connect},
			Msg
	end.

send_call({Session, _, Endpoint}, SendMsg, ack) ->
	Room = session_queue:lookup(Session),
	Message = {self(), post, string:join(["6", "", Endpoint, SendMsg], ":")},
	pid_sent(Message, Room);
send_call({Session, Type, Endpoint}, SendMsg, self) ->
	Room = session_queue:lookup(Session),
	Message = {self(), post, string:join([Type, "", Endpoint, SendMsg], ":")},
	pid_sent(Message, Room);

send_call({_, Type, Endpoint}, SendMsg, TargetSessiones = [_|_]) ->
	Message = {self(), post, string:join([Type, "", Endpoint, SendMsg], ":")},
	lists:foreach(fun(TargetSession) -> 
			Room = session_queue:lookup(TargetSession),
			pid_sent(Message, Room)
		end, TargetSessiones);

send_call({_, _, Endpoint}, SendMsg, {TargetSessions=[_|_], MessageType}) ->
	Message = {self(), post, string:join([MessageType, "", Endpoint, SendMsg], ":")},
	lists:foreach(fun(TargetSession) ->
						  Room = session_queue:lookup(TargetSession),
						  pid_sent(Message, Room)
				  end, TargetSessions);

send_call({_, _, Endpoint}, SendMsg, {TargetSession, MessageType}) ->
	Room = session_queue:lookup(TargetSession),
	Message = {self(), post, string:join([MessageType, "", Endpoint, SendMsg], ":")},
	pid_sent(Message, Room);

send_call({_, Type, Endpoint}, SendMsg, TargetSession) ->
	Room = session_queue:lookup(TargetSession),
	Message = {self(), post, string:join([Type, "", Endpoint, SendMsg], ":")},
	pid_sent(Message, Room).

pid_sent(Msg, Pid) ->
	Pid ! Msg.

gen_output(String) ->
	[DescList] = io_lib:format("~ts", [String]),
    erlang:iolist_to_binary(DescList).