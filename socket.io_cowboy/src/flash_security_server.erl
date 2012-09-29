-module(flash_security_server).
-export([start/1]).

start(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {reuseaddr, true}, {active, true}]),
    spawn(fun()-> connect(Listen) end).

connect(Listen)->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun()-> connect(Listen) end),
    loop(Socket).

loop(Socket)->
    receive
        {tcp, Socket, <<"<policy-file-request/>", 0>>} ->
            Reply = <<"<cross-domain-policy><allow-access-from domain=\"*\" to-ports=\"*\" /></cross-domain-policy>", 0>>,
            gen_tcp:send(Socket, Reply),
            loop(Socket);
        {tcp, closed, Socket}->
            lager:debug("server closed socket")
    end.