%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : 2012-9-17
%%% -------------------------------------------------------------------
-module(map_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/0]).
-export([new_message_queue/1, push_message/2, pop_message/1, delete_message/1]).
-export([add_session_pid/2, lookup_pid/1, delete_pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {message}).

%% ====================================================================
%% External functions
%% ====================================================================
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
%% add new message queue in mem
new_message_queue(Session) ->
	gen_server:call(?MODULE, {add_queue, Session, queue:new()}).
push_message(Session, Message) ->
	gen_server:call(?MODULE, {add_message, Session, Message}).
pop_message(Session) ->
	gen_server:call(?MODULE, {pop_message, Session}).
delete_message(Session) ->
	gen_server:call(?MODULE, {delete_message, Session}).

add_session_pid(Session, Pid) ->
	gen_server:call(?MODULE, {add_session_pid, Session, Pid}).
lookup_pid(Session) ->
	gen_server:call(?MODULE, {lookup_session_pid, Session}).
delete_pid(Session) ->
	gen_server:call(?MODULE, {delete_session_pid, Session}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{
		message = ets:new(message, [set])			
	}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({add_session_pid, Session, Pid}, From, State) ->
	Key = "pid_" ++ Session,
	Reply = ets:insert(State#state.message, {Key, Pid}),	
    {reply, Reply, State};

handle_call({lookup_session_pid, Session}, From, State) ->
	Key = "pid_" ++ Session,
	[{Key, Reply}] = ets:lookup(State#state.message, Key),
    {reply, Reply, State};

handle_call({delete_session_pid, Session}, From, State) ->
	Reply = ets:delete(State#state.message, "pid_" ++ Session),
    {reply, Reply, State};

handle_call({pop_message, Session}, From, State) ->
	Result = ets:lookup(State#state.message, Session),
	case Result of
		[] ->
			Reply = none;
		[{Session, Queue}] ->
			case queue:out(Queue) of
				{{value, Reply}, NewQueue} ->
					ets:insert(State#state.message, {Session, NewQueue});
				_ ->
					Reply = none
			end
	end,	
	
    {reply, Reply, State};
handle_call({delete_message, Session}, From, State) ->
	Reply = ets:delete(State#state.message, Session),	
    {reply, Reply, State};
handle_call({add_queue, Session, Queue}, From, State) ->
	Reply = ets:insert(State#state.message, {Session, Queue}),
    {reply, Reply, State};

handle_call({add_message, Session, Message}, From, State) ->
	[{Session, Queue}] = ets:lookup(State#state.message, Session),
	NewQueue = queue:in(Message, Queue),
	Reply = ets:insert(State#state.message, {Session, NewQueue}),
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

