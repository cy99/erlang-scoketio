-module(chat_demo).
-compile(export_all).
-export([]).

on_connect({[Session, MessageId, Endpoint, OriMessage], SendFn}) ->
	io:format("chat demo was called on_connect funtion with OriMsg : ~s and session id ~s ~n", [OriMessage, Session]).

on_disconnect({Session, Endpoint, SubMsgData}) ->
	io:format("the session(~s) with Endpoint(~s) is disconnected now ~n", [Session, Endpoint]),
	NickMap = register_spawn(),
	NickMap !{self(), delete, Session},
	ok.
	
nickname_spawn(NickMap) ->
	receive
		{From, {Key, Value}} ->
			From ! ets:insert(NickMap, {Key, Value}),
			nickname_spawn(NickMap);
		{From, tabName} ->
			From ! NickMap,
			nickname_spawn(NickMap);
		{From, void} ->
			void;
		{From, delete, Session} ->
			From !ets:delete(NickMap, Session),
			nickname_spawn(NickMap);
		{From, lookup, Session} ->
			[{_, Value}] = ets:lookup(NickMap, Session),
			Sessions = ets:foldl(fun({OneSession, _}, Arrs) ->
					[OneSession|Arrs]
				end, Arrs = [], NickMap),
			From ! {ok, Value, Sessions},
			nickname_spawn(NickMap);
		{From, getNicknames} ->
			NicknameList = ets:foldl(fun({_, Value}, Arrs) ->
											 Str = io_lib:format("~p:~p,", [Value, Value]),
											 Arrs ++ [Str] end, Arrs = [], NickMap),

			NickNameStr = lists:flatten(NicknameList),
			FormatNicknameStr = case string:len(NickNameStr) > 0 of
				true ->
					string:substr(NickNameStr, 1, string:len(NickNameStr)-1);
				false ->
					NickNameStr
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
	{_,D} = mochijson2:decode(Message),
	Key = proplists:get_value(<<"name">>, D),
	handle_event_name(Key, D, {Session, Type, Endpoint, Message, SendFn}),
	io:format("chat demo recevie message is ~s with session id ~s ~n", [Message, Session]).

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
	NickMap !{self(), {Session, NickNameStr}},
	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", NickNameStr ++ " connected"])),
	SendFn(Welcome),
	
	NickMap ! {self(), getNicknames},
	receive
		{ok, FormatNicknameStr, Sessions} -> ok
	end,
	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[{~s}]}", ["nicknames", FormatNicknameStr])),
	NewMessage = {self(), post, string:join(["5", "", Endpoint, NicknameNotice], ":")},
	lists:foreach(fun(OneSession) ->
						  OneSessionPid = session_queue:register(OneSession),
						  OneSessionPid ! NewMessage
					  end, Sessions);
	
%% 	SendFn(NicknameNotice);
handle_event_name(<<"user message">>, Json, {Session, Type, Endpoint, Message, SendFn}) ->
	[MessageBinary] = proplists:get_value(<<"args">>, Json),
	MsgTxtStr = lists:flatten(binary_to_list(MessageBinary)),
	NickMap = register_spawn(),
	NickMap !{self(), lookup, Session},
	receive
		{ok, Nickname, Sessions} -> ok
	end,
	
	%% 查找同一房间好友, 发送此消息

	JsonMessage = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[~p,~p]}", ["user message", Nickname, MsgTxtStr])),
	NewMessage = {self(), post, string:join(["5", "", Endpoint, JsonMessage], ":")},
	lists:foreach(fun(OneSession) ->
						  case OneSession =:= Session of
							  false ->
								  OneSessionPid = session_queue:register(OneSession),
								  OneSessionPid ! NewMessage;
							  true ->
								  true
						  end
						  end, Sessions);
%% 	SendFn(NewMessage);
handle_event_name(<<_>>, Json, {Session, Type, Endpoint, Message, SendFn}) ->
	io : format("got user message is ~s~n", [Message]),
	SendFn(Message).