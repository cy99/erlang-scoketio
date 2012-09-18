%% Author: Administrator
%% Created: 2012-9-18
%% Description: TODO: Add description to decode
-module(socketio_decode).
-compile(export_all).
-define(PATTERN, "(\\d):(\\d+\\+)?:(/[^:]*)?:?(.*)?").
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
decode_structure(Msg) ->
	Result = re:run(Msg, ?PATTERN, [{capture, all_but_first, list}]),
	case Result of
		{match, Captured} ->
			Captured;
		nomatch ->
			[]
	end.

decode(Msg) ->
	case decode_structure(Msg) of
		[] ->
			[];
		[Type, MessageId, Endpoint, Data] ->
			message({Msg, [Type, MessageId, Endpoint, Data]})
	end.

message({Msg, ["1", MessageId, Endpoint, Data]}) ->
	%% 触发初始化连接事件	
	[Msg];
message({Msg, ["5", MessageId, Endpoint, Data]}) ->
	%% 触发初始化连接事件	
	case string:len(MessageId) > 0 of
		true ->
			Ack = "6::" ++ Endpoint ++ ":" ++ MessageId ++ "[false]",
			NewMsg = string:join(["5", "", Endpoint, Data], ":"),
			Messages = [Ack, NewMsg];
		false ->
			Messages = []
	end;
message({Msg, [Type, MessageId, Endpoint, Data]}) ->
	[Msg].

%%
%% Local Functions
%%

