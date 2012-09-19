-module(session_queue).

-export([register/1]).

queue(Subscribed, Messages, Defined) ->
    receive
        {From, subscribe} ->
			case Messages of
				[] ->
					NewDefined = defined,
					NewMessages = [],
					
					NewSubscribed = if 
						Subscribed == false ->
							From ! first,
							true;
						true ->
							true
					end;
				[H|T] -> %% 若有消息，则发送
					From ! H,
					NewDefined = undefined,
					NewMessages = T,
					NewSubscribed = Subscribed
			end,
            queue(NewSubscribed, NewMessages, NewDefined);
        {From, unsubscribe} ->
            void;
        {From, end_connect} ->
            queue(Subscribed, Messages, undefined);
        {From, post, Message} ->
			case Defined of
				defined ->
					From ! Message,
					NewDefined = undefined,
					NewMessages = Messages;
				undefined ->
					NewDefined = Defined,
					NewMessages = [Message | Messages]
			end,
            queue(Subscribed, NewMessages, NewDefined);
        _Any ->
            queue(Subscribed, Messages, Defined)
    end.

register(Session) ->
    case map_server:lookup_pid(Session) of
        none ->
            io:format("create the room with spawn now ~n"),
            NewPid = spawn(fun() ->
                %% queue(init([]), [])
                queue(false, [], undefined)
            end),
			map_server:add_session_pid(Session, NewPid),
            NewPid;
        Pid ->
            Pid
    end.