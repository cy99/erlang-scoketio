%% Author: Administrator
%% Created: 2012-9-17
%% Description: TODO: Add description to uuid_tests
-module(uuid_tests).
%%-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

count_num_test() ->
	Max = 50,
	?assertEqual(Max, length(compute(Max, []))).

compute(Max, Rs) ->
	List = lists:seq(1, Max),
	lists:foreach(fun(X)-> [uuid:gen()|Rs]									
						  end, List).

compute(Max) ->
	List = lists:seq(1, Max),
	lists:foreach(fun(X)-> io:format("~p~n", [uuid:gen()])									
						  end, List).
		
