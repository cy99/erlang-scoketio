%% Author: Administrator
%% Created: 2012-9-27
%% Description: TODO: Add description to t
-module(t).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile(export_all).
-export([]).
-define(DB, DBName).
%%
%% API Functions
%%
v() ->
	?DB = ets:new(dbname, [set]),
	ets:insert(?DB, {1, "hello"}),
	io:format("~p~n", [ets:lookup(?DB, 1)]).


%%
%% Local Functions
%%

