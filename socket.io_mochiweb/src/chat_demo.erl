-module(chat_demo).
-export([on_connect/2, on_disconnect/2, on_message/2]).

on_connect({Session, MessageId, Endpoint, OriMessage}, SendFn) ->
	io:format("chat demo was called on_connect funtion with OriMsg : ~s and session id ~s ~n", [OriMessage, Session]).

on_disconnect({Session, Endpoint, SubMsgData}, SendFn) ->
	NickMap = register_spawn(),		
	NickMap ! {self(), lookup, Session},
	receive
		{ok, Nickname, _} -> ok
	end,
	NickMap ! {self(), delete, Session},
	
	NickMap ! {self(), getNicknames},
	receive
		{ok, FormatNicknameStr, LeftSessions} -> ok
	end,
	Type = "5",
	Announcement = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", Nickname ++ " disconnected"])),
	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[{~s}]}", ["nicknames", FormatNicknameStr])),
	SendFn(Announcement, {LeftSessions, Type}),
	SendFn(NicknameNotice, {LeftSessions, Type}).

on_message({Session, Type, MessageId, Endpoint, Message}, SendFn) ->
	case string:len(MessageId) > 0  of
		true ->
			Ack = MessageId ++ "[false]",
			 SendFn(Ack, ack);
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
	
	NickMap ! {self(), sessions},
	receive
		{ok, Sessions} -> Sessions
	end,
	SendFn(Welcome, {Sessions, Type}),
	
	NickMap ! {self(), getNicknames},
	receive
		{ok, FormatNicknameStr, Sessions} -> ok
	end,
	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[{~s}]}", ["nicknames", FormatNicknameStr])),
	SendFn(NicknameNotice, Sessions);
handle_event_name(<<"user message">>, Json, {Session, Type, Endpoint, Message, SendFn}) ->
	[MessageBinary] = proplists:get_value(<<"args">>, Json),
	MsgTxtStr = lists:flatten(binary_to_list(MessageBinary)),
	NickMap = register_spawn(),
	NickMap ! {self(), lookup, Session},
	receive
		{ok, Nickname, Sessions} -> ok
	end,
	
	JsonMessage = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\",\"~s\"]}", ["user message", Nickname, MsgTxtStr])),
	
	SendFn(JsonMessage, lists:delete(Session, Sessions));
handle_event_name(<<_>>, Json, {Session, Type, Endpoint, Message, SendFn}) ->
	SendFn(Message, self).

nickname_spawn(NickMap) ->
	receive
		{From, {Key, Value}} ->
			From ! ets:insert(NickMap, {Key, Value}),
			nickname_spawn(NickMap);
		{From, tabName} ->
			From ! NickMap,
			nickname_spawn(NickMap);
		{_, void} ->	void;
		{From, delete, Session} ->
			From ! ets:delete(NickMap, Session),
			nickname_spawn(NickMap);
		{From, lookup, Session} ->
			case ets:lookup(NickMap, Session) of
				[] ->
					void;
				[{_, Value}] ->
					Sessions = ets:foldl(fun({OneSession, _}, Arrs) ->
							[OneSession|Arrs]
						end, [], NickMap),
					From ! {ok, Value, Sessions}
			end,
			nickname_spawn(NickMap);
		{From, sessions} ->
			Sessions = ets:foldl(fun({OneSession, _}, Arrs) ->
					[OneSession|Arrs]
				end, [], NickMap),
			From ! {ok, Sessions},
			nickname_spawn(NickMap);
		{From, getNicknames} ->
			NicknameList = ets:foldl(fun({_, Value}, Arrs) ->
											 Str = lists:flatten(io_lib:format("\"~s\":\"~s\",", [Value, Value])),
											 Arrs ++ [Str] end, [], NickMap),
			NickNameStr = lists:flatten(NicknameList),
			FormatNicknameStr = case string:len(NickNameStr) > 0 of
				true ->
					string:substr(NickNameStr, 1, string:len(NickNameStr)-1);
				false -> NickNameStr
				end,
			Sessions = ets:foldl(fun({OneSession, _}, Arrs) ->
					[OneSession|Arrs]
				end, [], NickMap),
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