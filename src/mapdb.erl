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
					NewSessions = Sessions,
					%% check have old message
					case map_server:pop_message(Session) of
						Reply ->
							From ! Reply
					end;
				false ->
					From ! first,
					io:format("now sent msg 1++~n"),
					NewSessions = [Session | Sessions],
					map_server:new_message_queue(Session)
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
                                  end
						  end, Sessions),						
            room(Sessions, []);
        _Any ->
            room(Sessions, Pids)
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