-module(htmlfile).
-compile(export_all).
%% -export([do_get/1, do_post/1, timeout_call/1]).
-define(HEARBEAT_INTERVAL, socketio_web:get_env(heartbeat_interval)*1000).
-define(HEARBEAT_TIMEOUT, socketio_web:get_env(heartbeat_timeout)*1000).

do_get({Session, Req}) ->
 	Data = Req:parse_qs(),
	Room = session_queue:register(Session),
	case proplists:lookup("disconnect", Data) of
		{"disconnect", _} ->
			set_timeout(Room, Session,1),
			Msg = "";
		_ ->
			set_timeout(Room, Session, ?HEARBEAT_TIMEOUT),
			Room ! {self(), subscribe},
			Response = Req:ok({"text/html; charset=utf-8",
                                      [{"Server" ,"Mochiweb-Test"}, {"Connection", "keep-alive"}],
                                      chunked}),
			
			Response:write_chunk("<html><body><script>var _ = function (msg) { parent.s._(msg, document); };</script>"
								"                                                                                                                                                                               "),
			
			wait_data(Response),
			Room ! {self(), end_connect}
	end.

wait_data(Response) ->
    Msg = receive
        first ->
			"1::";			  
		Message ->
			Message
    end,
	
    Response:write_chunk(gen_output(Msg)),
    wait_data(Response).

gen_output(String) ->
	[DescList] = io_lib:format("<script>_('~ts');</script>", [String]),
    Bin = erlang:iolist_to_binary(DescList).

timeout_call({Session, Endpoint, Type}) ->
	Implement = map_server:lookup(Endpoint),
	Room = session_queue:register(Session),
	Implement:on_disconnect({Session, Endpoint, timeout}, fun(SendMsg, Others) ->
				send_call({Session, Type, Endpoint}, SendMsg, Others)
	end),
	Room ! {self(), Session, unsubscribe}.

set_timeout(Room, Session, Timeout) ->
	Room ! {self(),getEndpoint},
	receive
		Endpoint ->
			ok
	end,
	case is_atom(Endpoint) of
		false ->
			TimeRef = case timer:apply_after(Timeout, ?MODULE, timeout_call, [{Session, Endpoint, "5"}]) of
					{ok, TRef} ->
						TRef;
					{error, Reason} ->
						io:format("occurs error now ~p~n", [Reason]),
						undefined
				end,
			Room ! {self(), timeout, TimeRef};
		true ->
			void
	end.

do_post_msg({Session,Msg}) ->
	Room = session_queue:register(Session),
	{[Type, MessageId, Endpoint, SubMsgData]} = socketio_decode:decode(Msg),
	
	%% TODO 此处不是很方便，有待重构
	Implement = map_server:lookup(Endpoint),
	case Type of
		"5" ->
			Implement:on_message({Session, Type, MessageId, Endpoint, SubMsgData}, fun(SendMsg, Others) ->
				send_call({Session, Type, Endpoint}, SendMsg, Others)
			end);
		"1" ->
			Room ! {self(), endpoint, Endpoint},
			Room ! {self(), post, Msg},
			Implement:on_connect({Session, MessageId, Endpoint, SubMsgData}, fun(SendMsg, Others) ->
				send_call({Session, Type, Endpoint}, SendMsg, Others)
			end);
		"2" ->
			set_timeout(Room, Session, ?HEARBEAT_TIMEOUT),
			timer:send_after(?HEARBEAT_INTERVAL, "2::");		
		"0" ->
			Implement:on_disconnect({Session, Endpoint, SubMsgData}, fun(SendMsg, Others) ->
				send_call({Session, Type, Endpoint}, SendMsg, Others)
			end),
			Room ! {self(), Session, unsubscribe}
	end.

do_post({Session, Req}) ->
	Data = Req:recv_body(),
	Msg = binary_to_list(Data),
	
	do_post_msg({Session, Msg}),
	
	Req:ok({"text/plain; charset=utf-8", [{"server", "Mochiweb-Test"}], "1"});

do_post(_) ->
	io:format("missing any thing at all now~n").
%%
%% Local Functions
%%
send_call({Session, Type, Endpoint}, SendMsg, ack) ->
	Room = session_queue:register(Session),
	Message = {self(), post, string:join(["6", "", Endpoint, SendMsg], ":")},
	pid_sent(Message, Room);
send_call({Session, Type, Endpoint}, SendMsg, self) ->
	Room = session_queue:register(Session),
	Message = {self(), post, string:join([Type, "", Endpoint, SendMsg], ":")},
	pid_sent(Message, Room);

send_call({Session, Type, Endpoint}, SendMsg, TargetSessiones = [H|T]) ->
	Message = {self(), post, string:join([Type, "", Endpoint, SendMsg], ":")},
	lists:foreach(fun(TargetSession) -> 
			Room = session_queue:register(TargetSession),
			pid_sent(Message, Room)
		end, TargetSessiones);

send_call({Session, Type, Endpoint}, SendMsg, {TargetSessions=[H|T], MessageType}) ->
	Message = {self(), post, string:join([MessageType, "", Endpoint, SendMsg], ":")},
	lists:foreach(fun(TargetSession) ->
						  Room = session_queue:register(TargetSession),
						  pid_sent(Message, Room)
				  end, TargetSessions);

send_call({Session, Type, Endpoint}, SendMsg, {TargetSession, MessageType}) ->
	Room = session_queue:register(TargetSession),
	Message = {self(), post, string:join([MessageType, "", Endpoint, SendMsg], ":")},
	pid_sent(Message, Room);

send_call({Session, Type, Endpoint}, SendMsg, TargetSession) ->
	Room = session_queue:register(TargetSession),
	Message = {self(), post, string:join([Type, "", Endpoint, SendMsg], ":")},
	pid_sent(Message, Room).

pid_sent(Msg, Pid) ->
	Pid ! Msg.