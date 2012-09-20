-module(chat_demo).
-compile(export_all).
-export([]).

on_connect({[Session, MessageId, Endpoint, OriMessage], SendFn}) ->
	io:format("chat demo was called on_connect funtion with OriMsg : ~s and session id ~s ~n", [OriMessage, Session]).

on_disconnect({Session, Endpoint, SubMsgData}) ->
	io:format("the session(~s) with Endpoint(~s) is disconnected now ~n", [Session, Endpoint]),
	NickMap = register_spawn(),
	NickMap !{self(), delete, Session},
	%% delete the session from NickMap
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
			{Session, Value} = ets:lookup(NickMap, Session),
			From ! {ok, Value},
			nickname_spawn(NickMap);
		{From, getNicknames} ->
			NicknameList = ets:foldl(fun({_, Value}, Arrs) ->
											 Str = io_lib:format("{~p:~p}", [Value, Value]),
											 Arrs ++ [Str] end, Arrs = [], NickMap),
			io:format("Lists is ~p~n", [NicknameList]),
			From ! {ok, NicknameList},
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

on_message({Session, Message, SendFn}) ->
	{_,D} = mochijson2:decode(Message),
	Key = proplists:get_value(<<"name">>, D),
	handle_event_name(Key, D, {Session, Message, SendFn}),
	io:format("chat demo recevie message is ~s with session id ~s ~n", [Message, Session]).

%% TODO 全局函数
%% on_init() ->
%% on_shutdown(_) ->

%%
%% Local Functions
%%
handle_event_name(<<"nickname">>, Json, {Session, Message, SendFn}) ->
	[NicknameBinary] = proplists:get_value(<<"args">>, Json),
	NickNameStr = lists:flatten(binary_to_list(NicknameBinary)),
	
	NickMap = register_spawn(),
	NickMap !{self(), {Session, NickNameStr}},
	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", NickNameStr ++ " connected"])),
	SendFn(Welcome),
	
	NickMap ! {self(), getNicknames},
	receive
		{ok, NicknameList} -> ok
	end,
	
	io:format("NicknameList is ~p length = ~p ~n", [NicknameList, length(NicknameList)]),
	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[~s]}", ["nicknames", lists:flatten(NicknameList)])),
	SendFn(NicknameNotice);
handle_event_name(<<"user message">>, Json, {Session, Message, SendFn}) ->
	io : format("got user message is ~s~n", [Message]),
	[MessageBinary] = proplists:get_value(<<"args">>, Json),
	MsgTxtStr = lists:flatten(binary_to_list(MessageBinary)),
	NickMap = register_spawn(),
	NickMap !{self(), lookup, Session},
	receive
		{ok, Nickname} ->
			ok
	end,
	
	NewMessage = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[{~p:~p}]}", ["user message", Nickname, MsgTxtStr])),
	SendFn(NewMessage);
handle_event_name(<<_>>, Json, {Session, Message, SendFn}) ->
	io : format("got user message is ~s~n", [Message]),
	SendFn(Message).