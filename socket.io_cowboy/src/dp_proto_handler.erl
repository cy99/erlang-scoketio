%% Author: Administrator
%% Created: 2012-10-8
%% Description: TODO: Add description to dp_proto_handler
-module(dp_proto_handler).
-behaviour(cowboy_protocol).
-define(FLASH_REQ, <<"<policy-file-request/>\0">>).
-define(FLASH_FILE, <<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>">>).


-record(state, {
	socket :: inet:socket(),
	transport :: module()
}).

-export([start_link/4]).

%%
%% API Functions
%%
-spec start_link(pid(), ssl:sslsocket(), module(), []) -> {ok, pid()}.
start_link(_ListenerPid, Socket, Transport, []) ->
    Pid = spawn_link(?MODULE, init, [Socket, Transport]),
    {ok, Pid}.

%%
%% Local Functions
%%
init(Socket, Transport) ->
	io:format("***************************************************"),
    recv(#state{socket=Socket, transport=Transport}),
    ok.

recv(State = #state{socket=Socket, transport=Transport})->
%% 	io:format("Gose Here ***************************************************************************************"),
    Transport:setopts(Socket, [{active, once}]),
    receive
          {ok, ?FLASH_REQ}->
			  Transport:send(Socket, ?FLASH_FILE),
			  terminate(State);
          {ok, Other}->
			  ok;
          {error, _Reason} ->
			  terminate(State)
    end.

-spec terminate(#state{}) -> ok.
terminate(#state{socket=Socket, transport=Transport}) ->
	Transport:close(Socket),
	ok.