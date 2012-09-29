%% @author yongboy <yong.boy@gmail.com>
%% @copyright 2012 yongboy <yong.boy@gmail.com>
%% @doc socketio.

-module(htmlfile).
-extends(xhr_polling).
-export([do_get/1, do_post/1, timeout_call/1]).
-define(HEARBEAT_INTERVAL, socketio:get_env(heartbeat_interval)*1000).

%%
%% API Functions
%%
%% @spec do_get({Session, Req}) -> void
%% @doc server for do get method
do_get({Session, Req}) ->
	Room = session_queue:lookup(Session),
	case Room of
		undefined ->
			Msg = "7:::[\"Request Invalide\"]+[\"Please do not do that!\"]",
			cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain, charset=utf-8">>}], list_to_binary(Msg), Req);
		_ ->
			do_handle_get_msg({Session, Req}, Room)
	end.

%% @spec do_post(Any) -> void
%% @doc server for do post method, call xhr_polling::do_post(Any)
do_post(Any) ->
	?BASE_MODULE:do_post(Any).

%% @spec timeout_call(Any) -> void
%% @doc call xhr_polling::timeout_call(Any)
timeout_call(Any) ->
	?BASE_MODULE:time_call(Any).

%%
%% Local Functions
%%
do_handle_get_msg({Session, Req}, Room) ->
 	Disconnected = case cowboy_http_req:qs_val(<<"disconnect">>, Req) of
		{undefined, NewReg} -> false;
		{_, NewReg} -> true
	end,
	case Disconnected of
		true ->
			?BASE_MODULE:set_timeout(Room, Session, 1),
			cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain, charset=utf-8">>}], <<"">>, Req);
		false ->
			{ok, Req2} = cowboy_http_req:chunked_reply(200, [{<<"Content-Type">>, <<"text/plain, charset=utf-8">>}], Req),
			cowboy_http_req:chunk("<html><body><script>var _ = function (msg) { parent.s._(msg, document); };</script>                                                                                                                                                                                                                  ", 
								Req2),
			Room ! {self(), subscribe, ?MODULE},
			wait_data(Session, Room, Req2),
			lager:debug("htmlfile end_connect~n"),
			Room ! {self(), end_connect}
	end.

wait_data(Session, Room, Req2) ->
    Msg = receive
        first ->
			timer:send_after(?HEARBEAT_INTERVAL, Room, {self(), post, "2::"}),
			"1::";
		{_, _} ->
			wait_data(Session, Room, Req2);
		Message ->
			Message
    end,
	
	lager:debug("htmlfile Msg is ~p~n", [Msg]),
    cowboy_http_req:chunk(gen_output(Msg), Req2),
    wait_data(Session, Room, Req2).

gen_output(String) ->
	DescList = io_lib:format("<script>_(~p);</script>", [String]),
	list_to_binary(DescList).
%% 	lists:flatten(DescList).