-module(chat_demo).
-compile(export_all).
-export([]).
%% -define(Nicknames, ets:new(nicknames, [set])).

%% on_init() ->
%% 	Nickmap = ets:new(nicknames, [set]).

on_connect({[Session, MessageId, Endpoint, OriMessage], SendFn}) ->
%% 	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", "You are Welcome ~"])),
%% 	SendFn(Welcome),
	io:format("chat demo was called on_connect funtion with OriMsg : ~s and session id ~s ~n", [OriMessage, Session]).

%% on_disconnect(_) ->
%% 	.
nickname_spawn(NickMap) ->
	receive
		{From, {Key, Value}} ->
			From ! ets:insert(NickMap, {Key, Value}),
			nickname_spawn(NickMap);
		{From, tabName} ->
			From ! NickMap,
			nickname_spawn(NickMap);
		{From, getNicknames} ->
			NicknameList = ets:foldl(fun({_, Value}, Arrs) -> [Value|Arrs] end, Arrs = [], NickMap),
			io:format("Lists is ~p~n", [NicknameList]),
			From ! {ok, NicknameList},
			nickname_spawn(NickMap)
	end.

register_spawn() ->
	Pid = whereis(nicknames),
	if
		is_pid(Pid) ->
			Pid;
		true ->
			NewPid = spawn(fun() ->
								   NickMap = ets:new(nickmap, [set]),
								   nickname_spawn(NickMap)
						   end),
			register(nicknames, NewPid),
			NewPid
	end.

on_message({Session, Message, SendFn}) ->
%% 	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", "You are Welcome ~"])),
%% 	SendFn(Welcome),
	{_,D} = mochijson2:decode(Message),
	Key = proplists:get_value(<<"name">>, D),
	handle_event_name(Key, D, {Session, Message, SendFn}),
	io:format("chat demo recevie message is ~s with session id ~s ~n", [Message, Session]).
%% on_shutdown(_) ->
%% 	.
test() ->
	NickMap = register_spawn(),
%% 	[NicknameBinary] = proplists:get_value(<<"args">>, Json),
	NickNameStr = "firefox",
	NickMap ! {self(), {NickNameStr, NickNameStr}},
%% 	io:format("~ets insert result(~s) ~p~n", [NickNameStr, InsertResult]),
	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", NickNameStr ++ " connected"])),
%% 	SendFn(Welcome),
%% 	io:format("ets'firs content : ~p~n", [ets:first(nickmap)]),
	NickMap ! {self(), getNicknames},
	receive
		{ok, NicknameList} ->
			io:format("NicknameList is ~p~n", [NicknameList])
%% 			NicknameList = Lists
	end.
%% 	NicknameList = get_ets_strs(),
%% 	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":~p}", ["nicknames", NicknameList])).

%%
%% Local Functions
%%
get_ets_strs() ->
	NickMap = register_spawn(),
	NicknameList = ets:foldl(fun({_, Value}, Arrs) -> [Value|Arrs] end, Arrs = [], NickMap).

handle_event_name(<<"nickname">>, Json, {Session, Message, SendFn}) ->
%% 	"announcement", nickName + " connected"
	[NicknameBinary] = proplists:get_value(<<"args">>, Json),
	NickNameStr = lists:flatten(binary_to_list(NicknameBinary)),
	
	NickMap = register_spawn(),
	NickMap !{self(), {NickNameStr, NickNameStr}},
%% 	io : format("ets insert result(~s) ~p~n", [NickNameStr, InsertResult]),
	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", NickNameStr ++ " connected"])),
	SendFn(Welcome),
	
	NickMap ! {self(), getNicknames},
	receive
		{ok, NicknameList} ->
			io:format("NicknameList is ~p~n", [NicknameList])
	end,
	
%% 	NicknameList = get_ets_strs(),
	io:format("NicknameList is ~p length = ~p ~n", [NicknameList, length(NicknameList)]),
	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":~p}", ["nicknames", NicknameList])),
	SendFn(NicknameNotice);
handle_event_name(<<"user message">>, Json, {Session, Message, SendFn}) ->
	io : format("got user message is ~s~n", [Message]),
	SendFn(Message);
handle_event_name(<<_>>, Json, {Session, Message, SendFn}) ->
	io : format("got user message is ~s~n", [Message]),
	SendFn(Message).