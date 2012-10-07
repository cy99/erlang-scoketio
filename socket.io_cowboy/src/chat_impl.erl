-module(chat_impl).
-behaviour(socketio_impl).
-define(NickMap, chat_tab).

-export([on_init/1, on_connect/2, on_disconnect/2, on_message/2, on_destroy/1]).

on_init(_Name) ->
	lager:debug("now init ets tab ..."),
	ets:new(?NickMap, [set, protected, named_table, {read_concurrency, true}]).
on_destroy(_Name) ->
	void.

on_connect({Session, MessageId, Endpoint, OriMessage}, SendFn) ->
	lager:debug("chat demo was called on_connect funtion with OriMsg : ~s and session id ~s ~n", [OriMessage, Session]).

on_disconnect({Session, Endpoint, SubMsgData}, SendFn) ->
	Nickname = case ets:lookup(?NickMap, Session) of
				[] ->
					undefined;
				[{_, Value}] ->
					Value
			end,
	ets:delete(?NickMap, Session),
	NicknameList = ets:foldl(fun({_, Value}, Arrs) ->
											 Str = lists:flatten(io_lib:format("\"~s\":\"~s\",", [Value, Value])),
											 Arrs ++ [Str] end, [], ?NickMap),
	NickNameStr = lists:flatten(NicknameList),
	FormatNicknameStr = case string:len(NickNameStr) > 0 of
		true ->
			string:substr(NickNameStr, 1, string:len(NickNameStr)-1);
		false -> NickNameStr
	end,

	Sessions = ets:foldl(fun({OneSession, _}, Arrs) ->
					[OneSession|Arrs]
				end, [], ?NickMap),		
	Type = "5",
	Announcement = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", Nickname ++ " disconnected"])),
	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[{~s}]}", ["nicknames", FormatNicknameStr])),
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

%% TODO 全局函数
%% on_init() ->
%% on_shutdown(_) ->

%%
%% Local Functions
%%
handle_event_name(<<"nickname">>, Json, {Session, Type, Endpoint, Message, SendFn}) ->
	[NicknameBinary] = proplists:get_value(<<"args">>, Json),
	NickNameStr = lists:flatten(binary_to_list(NicknameBinary)),
	
	ets:insert(?NickMap, {Session, NickNameStr}),
	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", NickNameStr ++ " connected"])),
	
	Sessions = ets:foldl(fun({OneSession, _}, Arrs) ->
					[OneSession|Arrs]
				end, [], ?NickMap),
	SendFn(Welcome, {Sessions, Type}),
	NicknameList = ets:foldl(fun({_, Value}, Arrs) ->
											 Str = lists:flatten(io_lib:format("\"~s\":\"~s\",", [Value, Value])),
											 Arrs ++ [Str] end, [], ?NickMap),
	NickNameStr = lists:flatten(NicknameList),
	FormatNicknameStr = case string:len(NickNameStr) > 0 of
		true ->
			string:substr(NickNameStr, 1, string:len(NickNameStr)-1);
		false -> NickNameStr
	end,
	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[{~s}]}", ["nicknames", FormatNicknameStr])),
	SendFn(NicknameNotice, Sessions);

handle_event_name(<<"user message">>, Json, {Session, Type, Endpoint, Message, SendFn}) ->
	[MessageBinary] = proplists:get_value(<<"args">>, Json),
	MsgTxtStr = lists:flatten(binary_to_list(MessageBinary)),

	Nickname = case ets:lookup(?NickMap, Session) of
				[] ->
					undefined;
				[{_, Value}] ->
					Value
			end,
	Sessions = ets:foldl(fun({OneSession, _}, Arrs) ->
					[OneSession|Arrs]
				end, [], ?NickMap),	
	JsonMessage = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\",\"~s\"]}", ["user message", Nickname, MsgTxtStr])),
	SendFn(JsonMessage, lists:delete(Session, Sessions));
handle_event_name(<<_>>, Json, {Session, Type, Endpoint, Message, SendFn}) ->
	SendFn(Message, self).