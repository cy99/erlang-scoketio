-module(chat_impl).
-behaviour(socketio_impl).
-define(NickMap, chat_tab).

-export([on_init/1, on_connect/2, on_disconnect/2, on_message/2, on_destroy/1]).

on_init(_Name) ->
	ets:new(?NickMap, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]).
on_destroy(_Name) ->
	void.

on_connect({Session, _MessageId, _Endpoint, OriMessage}, _SendFn) ->
	lager:debug("chat demo was called on_connect funtion with OriMsg : ~s and session id ~s ~n", [OriMessage, Session]).

on_disconnect({Session, _Endpoint, _SubMsgData}, SendFn) ->
	Nickname = get_nickname(Session),
	ets:delete(?NickMap, Session),
	Sessions = get_sessions(),
	Type = "5",
	Announcement = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", Nickname ++ " disconnected"])),
	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[{~s}]}", ["nicknames", get_format_nicknames()])),
	SendFn(Announcement, {Sessions, Type}),
	SendFn(NicknameNotice, {Sessions, Type}).

on_message({Session, Type, MessageId, Endpoint, Message}, SendFn) ->
	case string:len(MessageId) > 0  of
		true ->
			Ack = MessageId ++ "[false]",
			SendFn(Ack, ack);
		false -> ok			
	end,
	{_, D} = mochijson2:decode(Message),
	Key = proplists:get_value(<<"name">>, D),
	handle_event_name(Key, D, {Session, Type, Endpoint, Message, SendFn}).

%%
%% Local Functions
%%
handle_event_name(<<"nickname">>, Json, {Session, Type, _Endpoint, _Message, SendFn}) ->
	[NicknameBinary] = proplists:get_value(<<"args">>, Json),
	NickNameStr = lists:flatten(binary_to_list(NicknameBinary)),
	ets:insert(chat_tab, {Session, NickNameStr}),
	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", NickNameStr ++ " connected"])),
	Sessions = get_sessions(),
	SendFn(Welcome, {Sessions, Type}),
	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[{~s}]}", ["nicknames", get_format_nicknames()])),
	SendFn(NicknameNotice, Sessions);

handle_event_name(<<"user message">>, Json, {Session, _Type, _Endpoint, _Message, SendFn}) ->
	[MessageBinary] = proplists:get_value(<<"args">>, Json),
	MsgTxtStr = lists:flatten(binary_to_list(MessageBinary)),

	Nickname = get_nickname(Session),
	Sessions = get_sessions(),	
	JsonMessage = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\",\"~s\"]}", ["user message", Nickname, MsgTxtStr])),
	SendFn(JsonMessage, lists:delete(Session, Sessions));
handle_event_name(<<_>>, _Json, {_Session, _Type, _Endpoint, Message, SendFn}) ->
	SendFn(Message, self).

get_sessions() ->
		ets:foldl(fun({OneSession, _}, Arrs) ->
					[OneSession|Arrs]
				end, [], ?NickMap).
get_nickname(Session) ->
	case ets:lookup(?NickMap, Session) of
		[] ->
			undefined;
		[{_, Value}] ->
			Value
	end.
get_format_nicknames() ->
	NicknameList = ets:foldl(fun({_, Value}, Arrs) ->
											 Str = lists:flatten(io_lib:format("\"~s\":\"~s\",", [Value, Value])),
											 Arrs ++ [Str] end, [], ?NickMap),
	NickNameStr = lists:flatten(NicknameList),
	case string:len(NickNameStr) > 0 of
		true ->
			string:substr(NickNameStr, 1, string:len(NickNameStr)-1);
		false -> NickNameStr
	end.