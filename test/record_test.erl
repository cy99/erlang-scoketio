-module(record_test).
-compile(export_all).
-record(state, {name, age=10}).

init() ->
	#state{name = "xiaomin"}.

test() ->
	State = init(),
	io:format("name is ~s age is ~p ~n", [State#state.name,State#state.age]).

t2() ->
	OriState = init(),
	State = OriState#state{age = 20},
	io:format("name is ~s age is ~p ~n", [State#state.name,State#state.age]).

t3() ->
	State = #state{},
	io:format("name is ~s age is ~p ~n", [State#state.name,State#state.age]).
t4() ->
	init(),
	State = #state{},
	io:format("name is ~s age is ~p ~n", [State#state.name,State#state.age]).