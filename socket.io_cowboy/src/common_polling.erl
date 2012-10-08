%% @author yongboy <yong.boy@gmail.com>
%% @copyright 2012 yongboy <yong.boy@gmail.com>
%% @doc socketio.

-module(common_polling).
-export([timeout_call/1, set_timeout/2, set_timeout/3, do_get_msg/1, do_post_msg/1]).
-define(HEARBEAT_INTERVAL, socketio:get_env(heartbeat_interval)*1000).
-define(HEARBEAT_TIMEOUT, socketio:get_env(heartbeat_timeout)*1000).

%%
%% API Functions
%%

%% @spec do_get_msg({Session, Disconnected}) -> Msg
%% @doc just export for htmlfile/jsonp module to call
do_get_msg({Session, Disconnected}) ->
	Room = session_queue:register(Session),
	case Room of
		undefined ->
			"7:::[\"Request Invalide\"]+[\"Please do not do that!\"]";
		_ ->
			do_handle_get_msg({Session, Disconnected}, Room)
	end.

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
	Room ! {self(), getEndpoint},
	Endpoint = receive
		{endpoint, TargetEndpoint} ->
			TargetEndpoint
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
		{error, _Reason} ->
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
			set_timeout(Room, Session, 1),
			"";
		false ->
			set_timeout(Room, Session, ?HEARBEAT_TIMEOUT),
			Room ! {self(), subscribe, ?MODULE},
			Msg = receive
					first ->
						"1::";
					Message ->
						Message
				after ?HEARBEAT_INTERVAL ->
						"8::"
				end,
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

send_call(_, _, []) ->
	void;
send_call({_, Type, Endpoint}, SendMsg, TargetSessiones = [_|_]) ->
	Message = {self(), post, string:join([Type, "", Endpoint, SendMsg], ":")},
	lists:foreach(fun(TargetSession) -> 
			Room = session_queue:lookup(TargetSession),
			pid_sent(Message, Room)
		end, TargetSessiones);

send_call(_, _, {[], _}) ->
	void;
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
	case Pid of
		undefined ->
			lager:error("Pid is undefined !"),
			ok;
		_ ->
			Pid ! Msg
	end.