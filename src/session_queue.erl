-module(session_queue).
-export([register/1]).
-record(state, {subscribed = false, messages = [], defined, timeRef, endpoint}).

queue(State) ->
    receive
        {From, subscribe} ->
			case State#state.messages of
				[] ->
					NewDefined = From,
					NewMessages = [],
					
					NewSubscribed = if
						State#state.subscribed == false ->
							From ! first,
							true;
						true ->
							true
					end;
				[H|T] -> %% 若有消息，则发送
					From ! H,
					NewDefined = undefined,
					NewMessages = T,
					NewSubscribed = true
			end,
            queue(State#state{subscribed = NewSubscribed, messages = NewMessages, defined = NewDefined});
        {From, Session, unsubscribe} ->
        	map_server:delete_pid(Session),
			case State#state.timeRef of
				undefined -> ok;
				_ -> timer:cancel(State#state.timeRef)
			end,
            void;
        {From, unsubscribe} ->
			case State#state.timeRef of
				undefined -> ok;
				_ -> timer:cancel(State#state.timeRef)
			end,
            void;
        {From, timeout, NewTimeRef} ->
            case State#state.timeRef of
				undefined -> ok;
				_ -> timer:cancel(State#state.timeRef)
			end,
			queue(State#state{timeRef = NewTimeRef});
        {From, end_connect} ->
            queue(State#state{defined=undefined});
		{From, endpoint, NewEndpoint} ->
			queue(State#state{endpoint=NewEndpoint});
		{From, getEndpoint} ->
			From ! State#state.endpoint,
            queue(State#state{});
        {From, post, Message} ->
			case State#state.defined of
				undefined ->
					NewDefined = State#state.defined,
					NewMessages = lists:merge([Message], State#state.messages);
				Pid ->
					Pid ! Message,
					NewDefined = undefined,
					NewMessages = State#state.messages
			end,
            queue(State#state{messages = NewMessages, defined = NewDefined});
        _Any ->
            queue(State#state{})
    end.

register(Session) ->
    case map_server:lookup_pid(Session) of
        none ->
            NewPid = spawn(fun() ->
                queue(#state{})
            end),
			map_server:add_session_pid(Session, NewPid),
            NewPid;
        Pid ->
            Pid
    end.