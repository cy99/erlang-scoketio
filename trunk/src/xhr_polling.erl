%% Author: Administrator
%% Created: 2012-9-15
%% Description: xhr-polling implemention
-module(xhr_polling).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([do_get/1, do_post/1]).

%%
%% API Functions
%%
do_get({Session, Req}) ->
 	Data = Req:parse_qs(),
	Room = session_queue:register(Session),
	case proplists:lookup("disconnect", Data) of
		{"disconnect", _} ->
			Room ! {self(), unsubscribe},
			map_server:delete_pid(Session),
			%% clear the session
			Msg = "";
		_ ->
			Room ! {self(), subscribe},
			Msg = do_handle(Session),
			Room ! {self(), end_connect}
	end,
	Req:ok({"text/plain; charset=utf-8", [{"server", "Mochiweb-Test"}], gen_output(Msg)}).

do_post({Session, Req}) ->
	Data = Req:recv_body(),
	Msg = binary_to_list(Data),
	Room = session_queue:register(Session),
	{[Type, MessageId, Endpoint, SubMsgData], Messages} = socketio_decode:decode(Msg),
	lists:foreach(fun(OneMsg) ->
						  case {Type, string:len(MessageId) > 0} of
							  {"5", true} ->
								 Room ! {self(), post, lists:nth(1, Messages)};
							  {"1", _} ->
								 Room ! {self(), post, OneMsg};
							  {_, _} ->
								  ok						  
						  end
						  end, Messages),
	
	OriMsg = case length(Messages) > 1 of
				 true ->
					 lists:nth(2, Messages);
				 false ->
					 lists:nth(1, Messages)
			 end,
	
	%% TODO 此处不是很方便，有待重构
	Implement = endpoint_server:lookup(Endpoint),
	case Type of
		"5" ->
			Implement:on_message({Session, Type, MessageId, Endpoint, SubMsgData, fun(SendMsg, Pid) ->
																						  TargetPid = if
																								  is_pid(Pid) -> Pid;
																								  true -> Room
																							  end,
												   TargetPid ! {self(), post, string:join(["5", "", Endpoint, SendMsg], ":")}
						  end});
		"1" ->
			Implement:on_connect({[Session, MessageId, Endpoint, SubMsgData], fun(SendMsg, Pid) ->
																					  TargetPid = if
																									  is_pid(Pid) -> Pid;
																									  true -> Room
																								  end,
												   TargetPid ! {self(), post, string:join(["5", "", Endpoint, SendMsg], ":")}
						  end});
		"0" ->
			Implement:on_disconnect({Session, Endpoint, SubMsgData}),
			Room ! {self(), unsubscribe},
			map_server:delete_pid(Session)
	end,
	
	Req:ok({"text/plain; charset=utf-8", [{"server", "Mochiweb-Test"}], "1"});

do_post(_) ->
	io:format("missing any thing at all now~n").
%%
%% Local Functions
%%
gen_output(String) ->
	[DescList] = io_lib:format("~ts", [String]),
    Bin = erlang:iolist_to_binary(DescList).
do_handle(Session) ->
	receive
		first ->
			Msg = "1::";
		Message ->
			Msg = Message
	after 20000 ->
			Msg = "8::"
	end.