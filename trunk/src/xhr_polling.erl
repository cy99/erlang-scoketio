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
	case proplists:lookup("disconnect", Data) of
		{"disconnect", _} ->
			Msg = "";
		none ->
			Msg = do_handle(Session)
	end,
	
%% 	case Type of
%% 		error ->
%% 			io:format("occur error now~n"),
%% 			Room ! {self(), Session, unsubscribe}
%% 	end,
	
	Req:ok({"text/plain; charset=utf-8", [{"server", "Mochiweb-Test"}], Msg}).

do_post({Session, Req}) ->
	Data = Req:recv_body(),
	Msg = binary_to_list(Data),
	Room = mapdb:get_the_room(),
	
	case string:substr(Msg, 3, 1) of
		":" ->
			Target = Msg;
		_ ->
			case re:run(Msg, ":\\d+\\+", [{capture, first, list}]) of
				{match, [Match]} ->
					Target0 = string:concat(string:concat("6::", Match), "[false]"),
					Room ! {self(), post, Target0}
			end,
			PlusIndex = string:chr(Msg, $+),
			Target = string:concat("5:", string:sub_string(Msg, PlusIndex + 1))
	end,
	
    Room ! {self(), post, Target},	
	io:format("$Receive data : ~p~n", [Target]),
	Req:ok({"text/plain; charset=utf-8", [{"server", "Mochiweb-Test"}], "1"});

do_post(_) ->
	io:format("missing any thing at all now~n").
%%
%% Local Functions
%%
do_handle(Session) ->
	Room = mapdb:get_the_room(),
	Room ! {self(), Session, subscribe},
	receive
		first ->
			Msg = "1::";
		Message ->
			io:format("receive msg ~p~n", [Message]),
			Msg = Message
	after 20000 ->
			Msg = "8::"
	end.