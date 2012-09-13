%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc socketio.

-module(socketio).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the socketio server.
start() ->
    socketio_deps:ensure(),
    ensure_started(crypto),
    application:start(socketio).


%% @spec stop() -> ok
%% @doc Stop the socketio server.
stop() ->
    application:stop(socketio).
