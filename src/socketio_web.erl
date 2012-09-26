-module(socketio_web).
-author('author <yongboy@gmail.com>').
-export([start/1, stop/0, loop/2]).
-export([get_env/1]).
-define(TIMEOUT, 20000).
-define(TRANSPORTS, [{"xhr-polling", xhr_polling}, {"jsonp-polling", jsonp_polling}, {"htmlfile", htmlfile}]).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
	uuid_server:start(),
	map_server:start(),

	%% register the demo implemention
	map_server:register("/chat", chat_demo),

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

get_transport(Transport) ->
	proplists:get_value(Transport, ?TRANSPORTS).

do_post(["socket.io", "1", Transport, Session], Req) ->
	NewTransport = get_transport(Transport),
	NewTransport:do_post({Session, Req});
do_post([], Req) ->
	Req:not_found();
do_post(_, Req) ->
	Req:respond({501, [], []}).

do_request(["socket.io", "1"], Req) ->
	UUID = uuid_server:gen(),
	Room = session_queue:register(UUID),
	xhr_polling:set_timeout(Room, UUID),
	Msg = io_lib:format("~s:~p:~p:~s", [UUID, get_env(heartbeat_timeout), get_env(close_timeout), get_env(allow_transports)]),
	Req:ok({"text/plain; charset=utf-8", [{"server", "socket.io server"}], Msg});
do_request(["socket.io", "1", Transport, Session], Req) ->
	NewTransport = get_transport(Transport),
	NewTransport:do_get({Session, Req});
do_request(_, Req) ->
	Req:not_found().

%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

get_env(Key) ->
	case application:get_env(Key) of
		{ok, Value} ->
			Value;		
		undefined ->
			undefined
	end.