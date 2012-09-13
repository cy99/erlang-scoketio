%% @author Mochi Media <dev@mochimedia.com>
%% @copyright socketio Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the socketio application.

-module(socketio_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for socketio.
start(_Type, _StartArgs) ->
    socketio_deps:ensure(),
    socketio_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for socketio.
stop(_State) ->
    ok.
