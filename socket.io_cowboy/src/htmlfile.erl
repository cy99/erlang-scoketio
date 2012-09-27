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
			Req:ok({"text/html; charset=utf-8",
                                      [{"Server" ,"socket.io server"}, {"Connection", "keep-alive"}],
                                      Msg});
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
 	Data = Req:parse_qs(),
	case proplists:lookup("disconnect", Data) of
		{"disconnect", _} ->
			?BASE_MODULE:set_timeout(Room, Session,1),
			Req:ok({"text/plain; charset=utf-8", [{"server", "socket.io server"}], ""});
		_ ->
			Response = Req:ok({"text/html; charset=utf-8",
                                      [{"Server" ,"socket.io server"}, {"Connection", "keep-alive"}],
                                      chunked}),
			
			Response:write_chunk("<html><body><script>var _ = function (msg) { parent.s._(msg, document); };</script>"
								"                                                                                                                                                                               "),
			Room ! {self(), subscribe, ?MODULE},
			wait_data(Session, Room, Response),
			Room ! {self(), end_connect}
	end.

wait_data(Session, Room, Response) ->
    Msg = receive
        first ->
			timer:send_after(?HEARBEAT_INTERVAL, Room, {self(), post, "2::"}),
			"1::";
		Message ->
			Message
    end,
	
    Response:write_chunk(gen_output(Msg)),
    wait_data(Session, Room, Response).

gen_output(String) ->
	DescList = io_lib:format("<script>_('~s');</script>", [String]),
	lists:flatten(DescList).