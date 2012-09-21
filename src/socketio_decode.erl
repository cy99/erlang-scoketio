%% Author: Administrator
%% Created: 2012-9-18
%% Description: TODO: Add description to decode
-module(socketio_decode).
-define(PATTERN, "(\\d):(\\d+\\+)?:(/[^:]*)?:?(.*)?").
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([decode/1]).

%%
%% API Functions
%%
decode(Msg) ->
	case decode_structure(Msg) of
		[] ->
			{[]};
		[Type, MessageId, Endpoint, Data] ->
			{[Type, MessageId, Endpoint, Data]};
		_ ->
			{[]}
	end.

%%
%% Local Functions
%%
decode_structure(Msg) ->
	Result = re:run(Msg, ?PATTERN, [{capture, all_but_first, list}]),
	case Result of
		{match, Captured} ->
			Captured;
		nomatch ->
			[]
	end.

%% message({Msg, ["1", MessageId, Endpoint, Data]}) ->
%% 	{["1", MessageId, Endpoint, Data], [Msg]};
%% message({Msg, ["5", MessageId, Endpoint, Data]}) ->
%% 	case string:len(MessageId) > 0 of
%% 		true ->
%% 			Ack = "6::" ++ Endpoint ++ ":" ++ MessageId ++ "[false]",
%% 			NewMsg = string:join(["5", "", Endpoint, Data], ":"),
%% 			Messages = [Ack, NewMsg];
%% 		false ->
%% 			Messages = [Msg]
%% 	end,
%% 	{["5", MessageId, Endpoint, Data], Messages};
%% message({Msg, [Type, MessageId, Endpoint, Data]}) ->
%% 	{[Type, MessageId, Endpoint, Data]}.