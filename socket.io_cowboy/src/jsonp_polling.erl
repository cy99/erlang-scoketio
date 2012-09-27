%% @author yongboy <yong.boy@gmail.com>
%% @copyright 2012 yongboy <yong.boy@gmail.com>
%% @doc socketio.

-module(jsonp_polling).
-extends(xhr_polling).
-export([do_get/1, do_post/1, timeout_call/1]).

%%
%% API Functions
%%

%% @spec do_get({Session, Req}) -> void
%% @doc server for do get method
do_get({Session, Req}) ->
	{I, Req2} = cowboy_http_req:qs_val(<<"i">>, Req),
	Disconnected = case cowboy_http_req:qs_val(<<"disconnect">>, Req) of
		{undefined, NewReg} -> false;
		{_, NewReg} -> true
	end,
	Msg = ?BASE_MODULE:do_get_msg({Session, Disconnected}),

	cowboy_http_req:reply(200, [
			{<<"Content-Type">>, 
			<<"application/x-javascript; charset=utf-8">>},
			{<<"X-XSS-Protection">>, <<"0">>}, 
			{<<"Connection">>, <<"keep-alive">>}
		], 
		gen_output(I, Msg), Req2).

%% @spec do_post(Any) -> void
%% @doc server for do post method
do_post({Session, Req}) ->
	Binary = list_to_binary(get_post_value(<<"d">>, Req)),
	OriMsg = binary_to_list(Binary),
	Msg2 = string:substr(OriMsg, 2, string:len(OriMsg)-2),
	Msg = re:replace(Msg2, "\\\\+", "", [global]),
	Result = ?BASE_MODULE:do_post_msg({Session, Msg}),
	cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain, charset=utf-8">>}], list_to_binary(Result), Req);
do_post(Any) ->
	?BASE_MODULE:do_post(Any).

%% @spec timeout_call(Any) -> void
%% @doc call xhr_polling::timeout_call(Any)
timeout_call(Any) ->
	?BASE_MODULE:time_call(Any).

%%
%% Local Functions
%%
gen_output(I, Msg) ->
	DescList = io_lib:format("io.j[~s]('~s');", [I, Msg]),
	list_to_binary(DescList).


get_post_values(Req) ->
    {Method, _} = cowboy_http_req:method(Req),
    get_post_values(Method, Req).

get_post_values('POST', Req) ->
    {Vals, _} = cowboy_http_req:body_qs(Req),
    Vals;
get_post_values(_, _) ->
    undefined.

get_post_value(Name, Req) ->
    PostVals = get_post_values(Req),
    extract_post_value(Name, PostVals).

extract_post_value(_, undefined) ->
    undefined;
extract_post_value(Name, PostVals) ->
    Matches = [X || X <- PostVals, Name =:= element(1,X)],
    process_post_value(Matches).

process_post_value([]) ->
    undefined;
process_post_value(Vals) ->
    {_, Result} = lists:unzip(Vals),
    Result.