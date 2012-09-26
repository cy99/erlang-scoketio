%% @author yongboy <yong.boy@gmail.com>
%% @copyright 2012 yongboy <yong.boy@gmail.com>
%% @doc socketio.

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
