%% Author: Administrator
%% Created: 2012-10-7
%% Description: TODO: Add description to socketio_impl
-module(socketio_impl).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([behaviour_info/1]).

%%
%% API Functions
%%
behaviour_info(callbacks) ->  
    [
	 {on_init, 1},  
     {on_connect, 2},
	 {on_message, 2},
	 {on_disconnect, 2},
	 {on_destroy, 1}
	];

behaviour_info(_Other) ->  
    undefined.