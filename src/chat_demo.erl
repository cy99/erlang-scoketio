%% Author: Administrator
%% Created: 2012-9-18
%% Description: TODO: Add description to chat_demo
-module(chat_demo).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile(export_all).
-export([]).

%%
%% API Functions
%%
on_connect({[Session, MessageId, Endpoint, OriMessage], SendFn}) ->
%% 	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", "You are Welcome ~"])),
%% 	SendFn(Welcome),
	io:format("chat demo was called on_connect funtion with OriMsg : ~s and session id ~s ~n", [OriMessage, Session]).

%% on_disconnect(_) ->
%% 	.

on_message({Session, Message, SendFn}) ->
%% 	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", "You are Welcome ~"])),
%% 	SendFn(Welcome),
	{_,D} = mochijson2:decode(Message),
	Key = proplists:get_value(<<"name">>, D),
	handle_event_name(Key, {Session, Message, SendFn}),
	io:format("chat demo recevie message is ~s with session id ~s ~n", [Message, Session]).
%% on_shutdown(_) ->
%% 	.


%%
%% Local Functions
%%
handle_event_name(<<"nickname">>, {Session, Message, SendFn}) ->
%% 	"announcement", nickName + " connected"
	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", "You are Welcome ~"])),
	SendFn(Welcome);
handle_event_name(<<"user message">>, {Session, Message, SendFn}) ->
	io : format("got user message is ~s~n", [Message]),
	SendFn(Message);
handle_event_name(<<_>>, {Session, Message, SendFn}) ->
	io : format("got user message is ~s~n", [Message]),
	SendFn(Message).