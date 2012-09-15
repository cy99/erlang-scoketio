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
					do_request(string:tokens(Path, "/"), Req)
			end;
		'POST' ->
			do_post(string:tokens(Path, "/"), Req);
        _ ->
            Req:respond({501, [], []})
    end.

%% http://10.95.20.172:9000/socket.io/1/xhr-polling/c0b16716-cb45-46b5-952d-848c7dd1ea64?t=1347500902596
%% 5:1+:/chat:{"name":"nickname","args":["firefox"]}
do_post(["socket.io", "1", Transport, Session], Req) ->
	NewTransport = re:replace(Transport, "-", "_"),
	NewTransport:do_post(Session, Req);
do_post([], Req) ->
	Req:not_found();
do_post(Any, Req) ->
	Req:respond({501, [], []}).

%% http://10.95.20.172:9000/socket.io/1/?t=1347500845159
do_request(["socket.io", "1"], Req) ->
	Msg = io_lib:format("~s:~p:~p:~s", [uuid:gen(), 60, 60, "xhr-polling"]),
	Req:ok({"text/plain; charset=utf-8", [{"server", "Mochiweb-Test"}], Msg});
do_request(["socket.io", "1", Transport, Session], Req) ->
	NewTransport = re:replace(Transport, "-", "_"),
	NewTransport:do_get(Session, Req);
%% 	Req:respond({302, [{"Location", Target}], "Redirecting to " ++ Target});
do_request(_, Req) ->
	Req:not_found().

%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.