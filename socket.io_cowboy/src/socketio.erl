%% @author yongboy <yong.boy@gmail.com>
%% @copyright 2012 yongboy <yong.boy@gmail.com>
%% @doc socketio.
-module(socketio).
-behaviour(application).
-export([start/0, start/2, stop/1]).
-export([get_env/1]).

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(cowboy),
	application:start(socketio).

start(_Type, _Args) ->
	Dispatch = [
		{'_', [
			{[<<"socket.io">>, <<"1">>], handshake_handler, []},
			{[<<"socket.io">>, <<"1">>, <<"websocket">>, '...'], websocket_handler, []},
			{[<<"socket.io">>, <<"1">>, <<"flashsocket">>, '...'], websocket_handler, []},
			{[<<"socket.io">>, <<"1">>, <<"htmlfile">>, '...'], htmlfile_handler, []},
			{[<<"socket.io">>, <<"1">>, <<"jsonp-polling">>, '...'], jsonp_handler, []},
			{[<<"socket.io">>, <<"1">>, <<"xhr-polling">>, '...'], xhr_handler, []},
			{['...'], cowboy_http_static, [
                    {directory, {priv_dir, ?MODULE, [<<"www">>]}},
                    {mimetypes, [
                        {<<".htm">>, [<<"text/html">>]},
                        {<<".html">>, [<<"text/html">>]},
                        {<<".css">>, [<<"text/css">>]},
                        {<<".js">>, [<<"application/x-javascript">>]},
                        {<<".jpeg">>, [<<"image/jpeg">>]},
                        {<<".jpg">>, [<<"image/jpeg">>]},
                        {<<".ico">>, [<<"image/x-icon">>]},
                        {<<".gif">>, [<<"image/gif">>]},
                        {<<".png">>, [<<"image/png">>]},
                        {<<".swf">>, [<<"application/x-shockwave-flash">>]}
                    ]}
                ]}
		]}
	],
	cowboy:start_listener(my_http_listener, get_env(netpool_acceptors),
		cowboy_tcp_transport, [{port, get_env(server_port)}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	
	cowboy:start_listener(my_https_listener, 1000,
		cowboy_ssl_transport, [
			{port, 8443}, {certfile, "priv/ssl/cert.pem"},
			{keyfile, "priv/ssl/key.pem"}, {password, "cowboy"}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	
	uuid_server:start(),
	map_server:start(),

	
	case get_env(flash_policy_port) of
		undefined ->
			void;
		Port ->
			cowboy:start_listener(dp_listener, 100,
              cowboy_tcp_transport, [{port, Port}],
              flash_security_handler, []
            )
	end,
	
	lager:start(),
	lager:set_loglevel(lager_console_backend, debug),
%% 	lager:set_loglevel(lager_file_backend, "console.log", debug),
	
	%% add your implemention here ...
	%% register the demo implemention
	ImplName = "/chat",
	map_server:register(ImplName, chat_impl),
	chat_impl:on_init(ImplName),

	socketio_sup:start_link().

stop(_State) ->
	ok.

%% @spec get_env(Key) -> Value | undefined
%% @doc get env value
get_env(Key) ->
	case application:get_env(?MODULE, Key) of
		{ok, Value} ->
			Value;		
		undefined ->
			undefined
	end.