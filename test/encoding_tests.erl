%% Author: Administrator
%% Created: 2012-9-17
%% Description: TODO: Add description to encoding_tests
-module(encoding_tests).
-include_lib("eunit/include/eunit.hrl").
-define(PATTERN, "(\\d):(\\d+\\+)?:(/[^:]*)?:?(.*)?").
-compile(export_all).

case_5_test() ->
	Msg = "5:::{\"name\":\"nickname\",\"args\":[\"firefox\"]}",
	List = decode(Msg),
	[
	 ?assertEqual(4, length(List)),
	 ?assertEqual("5", lists:nth(1, List)),
	 ?assertEqual("", lists:nth(2, List)),
	 ?assertEqual("", lists:nth(3, List)),
	 ?assertEqual("{\"name\":\"nickname\",\"args\":[\"firefox\"]}", lists:nth(4, List))
	 ].
case_5_plus_test() ->
	Msg = "5:1+:/chat:{\"name\":\"nickname\",\"args\":[\"firefox\"]}",
	List = decode(Msg),
	[
	 ?assertEqual(4, length(List)),
	 ?assertEqual("5", lists:nth(1, List)),
	 ?assertEqual("1+", lists:nth(2, List)),
	 ?assertEqual("/chat", lists:nth(3, List)),
	 ?assertEqual("{\"name\":\"nickname\",\"args\":[\"firefox\"]}", lists:nth(4, List))
	 ].

list_str_test() ->
	[
	?assertEqual("hello erlang", lists:flatten(["hello ", "erlang"]))
	].

t() ->
	Msg = "5:1+:/chat:{\"name\":\"nickname\",\"args\":[\"firefox\"]}",
	List = decode(Msg),
	msg(List).

msg([Type, MessageId, Endpoint, Data]) ->
	io:format("Message Type is ~s~nMessageId is ~s~nMessage Endpoint is ~s~nMessage Data is ~s~n", [Type, MessageId, Endpoint, Data]).


decode(Msg) ->
	Result = re:run(Msg, ?PATTERN, [{capture, all_but_first, list}]),
	case Result of
		{match, Captured} ->
			Captured;
		nomatch ->
			[]
	end.
%% {match, Captured} | match | nomatch


%% Msg = "5:1+:/chat:{\"name\":\"nickname\",\"args\":[\"firefox\"]}".

