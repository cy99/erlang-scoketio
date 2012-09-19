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
			Room ! {self(), unsubscribe},
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
	lists:foreach(fun(OneMsg) ->
						  io:format("OneMsg is ~s~n", [OneMsg]),
						  Room ! {self(), post, OneMsg}
				  end, socketio_decode:decode(Msg)),
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