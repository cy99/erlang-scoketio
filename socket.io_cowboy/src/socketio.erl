%% Feel free to use, reuse and abuse the code in this file.

-module(socketio).
-behaviour(application).
-export([start/0, start/2, stop/1]).
-export([get_env/1]).

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
%% 	application:start(ranch),
	application:start(cowboy),
%% 	application:start(static),
	application:start(socketio).

start(_Type, _Args) ->
	Dispatch = [
		{'_', [
%% 			{['...'], cowboy_http_static, [{directory, {priv_dir, static, []}}]}
%% 			{['...'], cowboy_http_static, [{directory, {priv_dir, <<"priv/www">>, []}}]}
			{[<<"socket.io">>, <<"1">>, <<"websocket">>, '...'], websocket_handler, []},
			{[<<"socket.io">>, <<"1">>, <<"flashsocket">>, '...'], websocket_handler, []},
			{[<<"socket.io">>, <<"1">>, '...'], transport_handler, []},
			{['...'], cowboy_static_handler, [{path, <<"priv/www">>}]}
		]}
	],
	cowboy:start_listener(my_http_listener, 1000,
		cowboy_tcp_transport, [{port, 8080}],
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

	%% register the demo implemention
	map_server:register("/chat", chat_demo),
	
	case get_env(flash_policy_port) of
		undefined ->
			void;
		Port ->
			flash_security_server:start(Port)
	end,
	
%% 	socketio_deps:ensure(),
	socketio_sup:start_link().

stop(_State) ->
	ok.

%% @spec get_env(Key) -> {ok, Value} | undefined
%% @doc get env value
get_env(Key) ->
	case application:get_env(?MODULE, Key) of
		{ok, Value} ->
			Value;		
		undefined ->
			undefined
	end.