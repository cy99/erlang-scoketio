-module(session_queue).

-export([register/1]).

queue(Subscribed, Messages, Defined, TimeRef, Endpoint) ->
    receive
        {From, subscribe} ->
			case Messages of
				[] ->
					NewDefined = From,
					%% io:format("subscribe receive Pid ~p~n", [NewDefined]),
					NewMessages = [],
					
					NewSubscribed = if
						Subscribed == false ->
							From ! first,
							true;
						true ->
							true
					end;
				[H|T] -> %% 若有消息，则发送
					%% io:format("from subscribe has message now ~s~n", [H]),
					From ! H,
					NewDefined = undefined,
					NewMessages = T,
					NewSubscribed = true
			end,
            queue(NewSubscribed, NewMessages, NewDefined, TimeRef, Endpoint);
        {From, Session, unsubscribe} ->
        	map_server:delete_pid(Session),
			case TimeRef of
				undefined -> ok;
				_ -> timer:cancel(TimeRef)
			end,
            void;
        {From, unsubscribe} ->
			case TimeRef of
				undefined -> ok;
				_ -> timer:cancel(TimeRef)
			end,
            void;
        {From, timeout, NewTimeRef} ->
            case TimeRef of
				undefined -> ok;
				_ -> timer:cancel(TimeRef)
			end,
			queue(Subscribed, Messages, Defined, NewTimeRef, Endpoint);
        {From, end_connect} ->
            queue(Subscribed, Messages, undefined, TimeRef, Endpoint);
		{From, endpoint, NewEndpoint} ->
            queue(Subscribed, Messages, undefined, TimeRef, NewEndpoint);
		{From, getEndpoint} ->
			From ! Endpoint,
            queue(Subscribed, Messages, undefined, TimeRef, Endpoint);		
        {From, post, Message} ->
			case Defined of
				undefined ->
					%% io:format("from post message with undefined is ~s~n", [Message]),
					NewDefined = Defined,
					NewMessages = Messages ++ [Message];
				Pid ->
					%% io:format("from post message with defined is ~s [Pid(~p)]~n", [Message, Pid]),
					Pid ! Message,
					NewDefined = undefined,
					NewMessages = Messages
			end,
            queue(Subscribed, NewMessages, NewDefined, TimeRef, Endpoint);
        _Any ->
            queue(Subscribed, Messages, Defined, TimeRef, Endpoint)
    end.

register(Session) ->
    case map_server:lookup_pid(Session) of
        none ->
            %% io:format("create the room with spawn now ~n"),
            NewPid = spawn(fun() ->
                %% queue(init([]), [])
                queue(false, [], undefined, undefined, undefined)
            end),
			map_server:add_session_pid(Session, NewPid),
            NewPid;
        Pid ->
            Pid
    end.