-module(chat_demo).
-compile(export_all).
-export([]).

on_connect({[Session, MessageId, Endpoint, OriMessage], SendFn}) ->
	io:format("chat demo was called on_connect funtion with OriMsg : ~s and session id ~s ~n", [OriMessage, Session]).

on_disconnect({Session, Endpoint, SubMsgData}) ->
	io:format("the session(~s) with Endpoint(~s) is disconnected now ~n", [Session, Endpoint]),
	NickMap = register_spawn(),
	NickMap !{self(), delete, Session}.
	
nickname_spawn(NickMap) ->
	receive
		{From, {Key, Value}} ->
			From ! ets:insert(NickMap, {Key, list_to_binary(Value)}),
			nickname_spawn(NickMap);
		{From, tabName} ->
			From ! NickMap,
			nickname_spawn(NickMap);
		{From, void} ->	void;
		{From, delete, Session} ->
			From !ets:delete(NickMap, Session),
			nickname_spawn(NickMap);
		{From, lookup, Session} ->
			[{_, Value}] = ets:lookup(NickMap, Session),
			Sessions = ets:foldl(fun({OneSession, _}, Arrs) ->
					[OneSession|Arrs]
				end, Arrs = [], NickMap),
			From ! {ok, binary_to_list(Value), Sessions},
			nickname_spawn(NickMap);
		{From, getNicknames} ->
			NicknameList = ets:foldl(fun({_, Value}, Arrs) ->
											 RealValue = binary_to_list(Value),
											 Str = lists:flatten(io_lib:format("\"~s\":\"~s\",", [RealValue, RealValue])),
											 Arrs ++ [Str] end, Arrs = [], NickMap),

			NickNameStr = lists:flatten(NicknameList),
			FormatNicknameStr = case string:len(NickNameStr) > 0 of
				true ->
					string:substr(NickNameStr, 1, string:len(NickNameStr)-1);
				false -> NickNameStr
				end,
			Sessions = ets:foldl(fun({OneSession, _}, Arrs) ->
					[OneSession|Arrs]
				end, Arrs = [], NickMap),
			From ! {ok, FormatNicknameStr, Sessions},
			nickname_spawn(NickMap)
	end.

register_spawn() ->
	Pid = whereis(nicknames),
	if
		is_pid(Pid) -> Pid;
		true ->
			NewPid = spawn(fun() ->
								   NickMap = ets:new(nickmap, [set]),
								   nickname_spawn(NickMap)
						   end),
			register(nicknames, NewPid),
			NewPid
	end.

on_message({Session, Type, MessageId, Endpoint, Message, SendFn}) ->
	case string:len(MessageId) > 0  of
		true ->
			Ack = MessageId ++ "[false]",
			 SendFn(Ack, parent, "6");
		false -> ok			
	end,
	{_,D} = mochijson2:decode(Message),
	Key = proplists:get_value(<<"name">>, D),
	handle_event_name(Key, D, {Session, Type, Endpoint, Message, SendFn}).

%% TODO 全局函数
%% on_init() ->
%% on_shutdown(_) ->

%%
%% Local Functions
%%
handle_event_name(<<"nickname">>, Json, {Session, Type, Endpoint, Message, SendFn}) ->
	[NicknameBinary] = proplists:get_value(<<"args">>, Json),
	NickNameStr = lists:flatten(binary_to_list(NicknameBinary)),
	
	NickMap = register_spawn(),
	NickMap ! {self(), {Session, NickNameStr}},
	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", NickNameStr ++ " connected"])),
	SendFn(Welcome, parent, Type),
	
	NickMap ! {self(), getNicknames},
	receive
		{ok, FormatNicknameStr, Sessions} -> ok
	end,
	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[{~s}]}", ["nicknames", FormatNicknameStr])),
	lists:foreach(fun(OneSession) ->
						  OneSessionPid = session_queue:register(OneSession),
						  SendFn(NicknameNotice, OneSessionPid, Type)
					  end, Sessions);
handle_event_name(<<"user message">>, Json, {Session, Type, Endpoint, Message, SendFn}) ->
	[MessageBinary] = proplists:get_value(<<"args">>, Json),
	MsgTxtStr = lists:flatten(binary_to_list(MessageBinary)),
	NickMap = register_spawn(),
	NickMap !{self(), lookup, Session},
	receive
		{ok, Nickname, Sessions} -> ok
	end,
	
	JsonMessage = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\",\"~s\"]}", ["user message", Nickname, MsgTxtStr])),
	lists:foreach(fun(OneSession) ->
						  case OneSession =:= Session of
							  false ->
								  OneSessionPid = session_queue:register(OneSession),
								  SendFn(JsonMessage, OneSessionPid, Type);
							  true -> true
						  end
						  end, Sessions);
handle_event_name(<<_>>, Json, {Session, Type, Endpoint, Message, SendFn}) ->
	SendFn(Message, parent, Type).