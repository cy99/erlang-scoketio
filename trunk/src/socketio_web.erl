-module(socketio_web).
-author('author <yongboy@gmail.com>').
-export([start/1, stop/0, loop/2]).
-define(TIMEOUT, 20000).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{max, 1000000}, {name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			case filelib:is_regular(filename:join([DocRoot, Path])) of
				true ->
					Req:serve_file(Path, DocRoot);
				false ->
					do_request(string:tokens(Path, "/"), Req, DocRoot)
			end;
		'POST' ->
			do_post(string:tokens(Path, "/"), Req, DocRoot);
        _ ->
            Req:respond({501, [], []})
    end.

%% http://10.95.20.172:9000/socket.io/1/xhr-polling/c0b16716-cb45-46b5-952d-848c7dd1ea64?t=1347500902596
%% 5:1+:/chat:{"name":"nickname","args":["firefox"]}
do_post(["socket.io", "1", Transport, Session], Req, DocRoot) ->
	Data = Req:recv_body(),
	Msg = binary_to_list(Data),
	Room = mapdb:get_the_room(),
	
	case string:substr(Msg, 3, 1) of
		":" ->
			Target = Msg;
		_ ->
			PlusIndex = string:chr(Msg, $+),
			case re:run("5:30+:[]", ":\\d+\\+", [{capture, first, list}]) of
				{match, [Match]} ->
					Target0 = string:concat("6::", Match, "[false]"),
					Room ! {self(), post, Target0}
			end,
			Target = string:concat("5:", string:sub_string(Msg, PlusIndex + 1))
	end,
	
    Room ! {self(), post, Target},	
	io:format("Receive data : ~p~n", [Target]),
	Req:ok({"text/plain; charset=utf-8", [{"server", "Mochiweb-Test"}], "1"});

do_post([], Req, DocRoot) ->
	Req:not_found();
do_post(Any, Req, DocRoot) ->
	Req:respond({501, [], []}).

%% http://10.95.20.172:9000/socket.io/1/?t=1347500845159
do_request(["socket.io", "1"], Req, DocRoot) ->
	Msg = io_lib:format("~s:~p:~p:~s", [uuid:gen(), 60, 60, "xhr-polling"]),
	Req:ok({"text/plain; charset=utf-8", [{"server", "Mochiweb-Test"}], Msg});

do_request(["socket.io", "1", Transport, Session], Req, DocRoot) ->
	Room = mapdb:get_the_room(),
    Room ! {self(), Session, subscribe},
	
	receive
	%%	first ->
	%%		{Type, Message} = {ok, "1::"};
		first ->
			Msg = "1::";
		Message ->
			Msg = Message,
			io:format("receive msg ~p~n", [Msg])
	after 20000 ->
			Msg = "8::"
	end,
	
%% 	case Type of
%% 		error ->
%% 			io:format("occur error now~n"),
%% 			Room ! {self(), Session, unsubscribe}
%% 	end,
	
	Req:ok({"text/plain; charset=utf-8", [{"server", "Mochiweb-Test"}], Msg});
	
%% do_request(["redi"], Req, DocRoot) ->
%% 	Target = "http://www.baidu.com/",
%% 	Req:respond({302, [{"Location", Target}], "Redirecting to " ++ Target});
do_request(_, Req, DocRoot) ->
	Req:not_found().

feed(Response, Path, N) ->
	receive
		%% to do nothing 
	after 10000 ->
		%% get_time() 返回 List， 需要设置 ~s,若使用~p/~w则被“”包含
		Msg = io_lib:format("<div>Chunk ~w for id ~s @ ~s</div>\n", [N, Path, get_time()]),
		Response:write_chunk(Msg)
	end,
	
	if N < 3 ->
		   feed(Response, Path, N+1);
	   true ->
		   Bye = io_lib:format("<div><strong>Done @ ~s!</strong></div>\n", [get_time()]),
		   Response:write_chunk(Bye),
		   %% 发送空数据，然后结束
		   Response:write_chunk([])
	   end.

get_time() ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
	lists:flatten(
	  io_lib:format("~p-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Year, Month, Day, Hour, Minute, Second])
	).

get2() ->
	Msg = io_lib:format("<div><strong>Done @ ~s!</strong></div>\n", [get_time()]),
	io:format(Msg),
	Msg2 = [],
	Len = iolist_size(Msg2),
	io:format("Len is ~p~n", [Len]),
	ok.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.