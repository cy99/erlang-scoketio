-module(mapdb).

-export([get_the_room/0]).
-record(state, {pid2id}).

init([]) ->
    #state{pid2id = ets:new(user, [set])}.

room(Sessions, Pids) ->
    receive
        {From, Session, subscribe} ->
			case lists:member(Session, Sessions) of 
				true ->
					%% io:format("has the data now~n"),
					NewSessions = Sessions;
				false ->
					From ! first,
					io:format("now sent msg 1++~n"),
					NewSessions = [Session | Sessions]
			end,
			
            room(NewSessions, [From | Pids]);
        {From, Session, unsubscribe} ->
            From ! unsubscribed,
            room(Sessions, Pids -- [From]);
        {From, post, Message} ->
            From ! posted,
            %% check if the pid has been romoved form Pids


			lists:foreach(fun(Session) ->
                                  Pid = map_server:lookup_pid(Session),
                                  case lists:member(Pid, Pids) of
                                        true -> Pid ! Message;
                                        false -> map_server:push_message(Session, Message)
                                  end,
								  %% map_server:push_message(Session, Message)
						  end, Sessions),						
%%             lists:foreach(fun(Pid) ->
%%                     Pid ! Message
%%                 end, Pids),
            room(Sessions, []);
        _Any ->
            room(Sessions, Pids)
    end.

room2(State, Users) ->
    receive
        {From, subscribe} ->
            From ! subscribed,
            room(State, [From | Users]);
        {From, Session, subscribe} ->
            From ! subscribed,
			
			case ets:lookup(State#state.pid2id, Session) of
				[{_, _}] ->
					io:format("has the data now"),
					ok;
				[] ->
					ets:insert(State#state.pid2id, {Session, Session}),
					io:format("now sent msg 1++"),
					From ! "1++"
			end,
			
            room(State, [From | Users]);
        {From, Session, unsubscribe} ->
            From ! unsubscribed,
			ets:delete(State#state.pid2id, Session),
            room(State, Users -- [From]);
        {From, post, Message} ->
            From ! posted,
            lists:foreach(fun(User) ->
                    User ! Message
                end, Users),
            room(State, []);
        _Any ->
            room(State, Users)
    end.

get_the_room() ->
    Pid = whereis(theroom),
    if
        is_pid(Pid) ->
            Pid;
        true ->
            io:format("create the room with spawn now ~n"),
            NewPid = spawn(fun() ->
                %% room(init([]), [])
                room([], [])
            end),
            register(theroom, NewPid),
            NewPid
    end.