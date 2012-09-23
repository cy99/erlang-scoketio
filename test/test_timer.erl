-module(test_timer).
-compile(export_all).

t() ->
	% send_after(Time, Pid, Message) -> {ok, TRef} | {error, Reason}
	case timer:send_after(10000, cancel) of
		{ok, TRef} ->
			io:format("has send the timer Message now ~n"),
			io:format("wait some time now ...~n"),
			receive
				Message ->
					io:format("receive message is ~p~n", [Message])
			after 9800 ->
				io:format("now cancel the timer with TimeRef ~p~n", [TRef])
			end;
		{error, Reason} ->
			io:format("occurs error now ~p~n", [Reason])
	end.
	