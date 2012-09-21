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
	%% 	Room = mapdb:get_the_room(),
	Room = session_queue:register(Session),
	case proplists:lookup("disconnect", Data) of
		{"disconnect", _} ->
%% 			Implement = endpoint_server:lookup(Endpoint),
			Room ! {self(), unsubscribe},
			map_server:delete_pid(Session),
			%% clear the session
%% 			map_server:delete_message(Session),
%% 			map_server:delete_pid(Session),
			Msg = "";
		_ ->
			Room ! {self(), subscribe},
			Msg = do_handle(Session),
			Room ! {self(), end_connect}
	end,
	io:format("now ouput the response is ~s [pid=~p] ~n", [Msg, self()]),	
	Req:ok({"text/plain; charset=utf-8", [{"server", "Mochiweb-Test"}], Msg}).

do_post({Session, Req}) ->
	Data = Req:recv_body(),
	Msg = binary_to_list(Data),
%% 	Room = mapdb:get_the_room(),
	Room = session_queue:register(Session),
	{[Type, MessageId, Endpoint, SubMsgData], Messages} = socketio_decode:decode(Msg),
	lists:foreach(fun(OneMsg) ->
						  case {Type, string:len(MessageId) > 0} of
							  {"5", true} ->
								 Room ! {self(), post, lists:nth(1, Messages)};
							  {"1", _} ->
								 io:format("OneMsg is ~s~n", [OneMsg]),
								 Room ! {self(), post, OneMsg};
							  {_, _} ->
								  ok						  
						  end
						  end, Messages),
	
	OriMsg = case length(Messages) > 1 of
				 true ->
%% 					 Room ! {self(), post, lists:nth(1, Messages)},
					 lists:nth(2, Messages);
				 false ->
					 lists:nth(1, Messages)
			 end,
	
	%% TODO 此处不是很方便，有待重构
	Implement = endpoint_server:lookup(Endpoint),
	case Type of
		"5" ->
			Implement:on_message({Session, Type, MessageId, Endpoint, SubMsgData, fun(SendMsg) ->
												   %% SendMsg需要为json类型字符串 
												   io:format("5 --> call back got message is ~s~n", [SendMsg]),
												   Room ! {self(), post, string:join(["5", "", Endpoint, SendMsg], ":")}
						  end});
		"1" ->
			Implement:on_connect({[Session, MessageId, Endpoint, SubMsgData], fun(SendMsg) ->
												   %% SendMsg需要为json类型字符串
												   io:format("1 --> call back got message is ~s~n", [SendMsg]),
												   Room ! {self(), post, string:join(["5", "", Endpoint, SendMsg], ":")}
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
do_handle(Session) ->
	receive
		first ->
			Msg = "1::";
		Message ->
			io:format("receive msg ~p~n", [Message]),
			Msg = Message
	after 20000 ->
			Msg = "8::"
	end.