-module(xhr_polling).
-export([do_get/1, do_post/1]).

do_get({Session, Req}) ->
 	Data = Req:parse_qs(),
	Room = session_queue:register(Session),
	case proplists:lookup("disconnect", Data) of
		{"disconnect", _} ->
			Room ! {self(), unsubscribe},
			map_server:delete_pid(Session),
			%% clear the session
			Msg = "";
		_ ->
			Room ! {self(), subscribe},
			Msg = do_handle(Session),
			Room ! {self(), end_connect}
	end,
	Req:ok({"text/plain; charset=utf-8", [{"server", "Mochiweb-Test"}], gen_output(Msg)}).

do_post({Session, Req}) ->
	Data = Req:recv_body(),
	Msg = binary_to_list(Data),
	Room = session_queue:register(Session),
	{[Type, MessageId, Endpoint, SubMsgData]} = socketio_decode:decode(Msg),
	
	%% TODO 此处不是很方便，有待重构
	Implement = endpoint_server:lookup(Endpoint),
	case Type of
		"5" ->
			Implement:on_message({Session, Type, MessageId, Endpoint, SubMsgData}, fun(SendMsg, Others) ->
				send_call({Session, Type, Endpoint}, SendMsg, Others)
			end);
		"1" ->
			Room ! {self(), post, Msg},
			Implement:on_connect({Session, MessageId, Endpoint, SubMsgData}, fun(SendMsg, Others) ->
				send_call({Session, Type, Endpoint}, SendMsg, Others)
			end);
		"0" ->
			Implement:on_disconnect({Session, Endpoint, SubMsgData}, fun(SendMsg, Others) ->
				send_call({Session, Type, Endpoint}, SendMsg, Others)
			end),
			Room ! {self(), unsubscribe},
			map_server:delete_pid(Session)
	end,
	
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

send_call({Session, Type, Endpoint}, SendMsg, TargetSession) ->
	Room = session_queue:register(TargetSession),
	Message = {self(), post, string:join([Type, "", Endpoint, SendMsg], ":")},
	pid_sent(Message, Room);

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

send_call({Session, Type, Endpoint}, SendMsg, _) ->
	Room = session_queue:register(Session),
	Message = {self(), post, string:join([Type, "", Endpoint, SendMsg], ":")},
	pid_sent(Message, Room).

pid_sent(Msg, Pid) ->
	Pid ! Msg.

gen_output(String) ->
	[DescList] = io_lib:format("~ts", [String]),
    Bin = erlang:iolist_to_binary(DescList).

do_handle(Session) ->
	receive
		first ->
			"1::";
		Message ->
			Message
	after 20000 ->
			"8::"
	end.