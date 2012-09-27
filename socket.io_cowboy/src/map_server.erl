%% @author yongboy <yong.boy@gmail.com>
%% @copyright 2012 yongboy <yong.boy@gmail.com>
%% @doc socketio.
-module(map_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/0]).
-export([add_session_pid/2, lookup_pid/1, delete_pid/1]).
-export([register/2, unregister/1, lookup/1]).
-export([get_env/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {session, endpoint}).

%% ====================================================================
%% External functions
%% ====================================================================
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @spec get_env(Key) -> {ok, Value} | undefined
%% @doc get env value
get_env(Key) ->
	case application:get_env(Key) of
		{ok, Value} ->
			Value;		
		undefined ->
			undefined
	end.

%% endpoint
%% @spec register(Endpoint, Implement) -> Boolean
%% @doc register endpoint with its implemention, when client call the endpoint, the server will execute the relation implement
register(Endpoint, Implement) ->
	gen_server:call(?MODULE, {register, Endpoint, Implement}).
%% @spec unregister(Endpoint) -> true
%% @doc delete the endpoint with its implemention
unregister(Endpoint) ->
	gen_server:call(?MODULE, {unregister, Endpoint}).

%% @spec lookup(Endpoint) -> Endpoint | none
%% @doc look up the endpoint's implemention, if found not, return none
lookup(Endpoint) ->
	gen_server:call(?MODULE, {lookup, Endpoint}).

%% session
%% @spec add_session_pid(Session, Pid) -> true
%% @doc insert into the truple {Session, Pid}
add_session_pid(Session, Pid) ->
	gen_server:call(?MODULE, {add_session_pid, Session, Pid}).

%% @spec lookup_pid(Session) -> Pid | undefined
%% @spec look up the Pid by Session
lookup_pid(Session) ->
	gen_server:call(?MODULE, {lookup_session_pid, Session}).

%% @spec delete_pid(Session) -> true
%% @doc delete truple {Session, Pid} by Session
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
		session = ets:new(session, [set]),
		endpoint = ets:new(endpoint, [set])
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

handle_call({register, Endpoint, Implement}, From, State) ->
	Reply = ets:insert(State#state.endpoint, {Endpoint, Implement}),
    {reply, Reply, State};

handle_call({lookup, Endpoint}, From, State) ->
	Reply = case ets:lookup(State#state.endpoint, Endpoint) of
		[{Key, Value}] ->
			Value;
		[] ->
			 none
	end,
    {reply, Reply, State};

handle_call({unregister, Endpoint}, From, State) ->
	Reply = ets:delete(State#state.endpoint, Endpoint),
    {reply, Reply, State};


handle_call({add_session_pid, Session, Pid}, From, State) ->
	Reply = ets:insert(State#state.session, {Session, Pid}),	
    {reply, Reply, State};

handle_call({lookup_session_pid, Session}, From, State) ->
	Result = ets:lookup(State#state.session, Session),
	case Result of
		[{Key, Reply}] ->
			Reply;
		[] ->
			 Reply = undefined
	end,
    {reply, Reply, State};

handle_call({delete_session_pid, Session}, From, State) ->
	Reply = ets:delete(State#state.session, Session),
    {reply, Reply, State};

handle_call(_, From, State) ->
	Reply = ok,
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

