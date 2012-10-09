%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : 2012-10-9
%%% -------------------------------------------------------------------
-module(session_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/0]).
-export([call/1, cast/1]).
-export([register/1, unregister/1, check/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(session, {subscribed = false, messages = [], defined, timeRef, endpoint, transport}).
-define(NickMap, session_tab).
-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

register(SessionId) ->
	gen_server:cast(?MODULE, {register, SessionId}).
unregister(SessionId) ->
	gen_server:cast(?MODULE, {unregister, SessionId}).

check(SessionId) ->
	gen_server:call(?MODULE, {lookup, SessionId}).

call(Request) ->
	gen_server:call(?MODULE, Request).

cast(Request) ->
	gen_server:cast(?MODULE, Request).

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
	ets:new(?NickMap, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, #state{}}.

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
handle_call({SessionId, getEndpoint}, _From, _State) ->
	Session = get_session(SessionId),
    Reply = Session#session.endpoint,
    {reply, Reply, _State};
handle_call({lookup, SessionId}, _From, _State) ->
	Reply = case ets:lookup(?NickMap, SessionId) of
		[] -> false;
		[_] -> true
	end,
    {reply, Reply, _State};
handle_call(Msg, _From, _State) ->
    Reply = ok,
    {reply, Reply, _State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({register, SessionId}, State) ->
	ets:insert(?NickMap, {SessionId, #session{}}),
    {noreply, State};
handle_cast({unregister, SessionId}, State) ->
	Session = get_session(SessionId),
	case Session#session.timeRef of
		undefined -> ok;
		_ -> timer:cancel(Session#session.timeRef)
	end,
	ets:delete(?NickMap, SessionId),
    {noreply, State};
handle_cast({SessionId, From, subscribe, Transport}, _State) ->
	Session = get_session(SessionId),
	case Session#session.messages of
		[] ->
			NewDefined = From,
			NewMessages = [],
			
			NewSubscribed = if
				Session#session.subscribed == false ->
					From ! {reply, first},
						true;
				true ->
						true
			end;
		[H|T] ->
			From ! {reply, H},
			NewDefined = undefined,
			NewMessages = T,
			NewSubscribed = true
	end,
	ets:insert(?NickMap, {SessionId, Session#session{subscribed = NewSubscribed, messages = NewMessages, defined = NewDefined, transport = Transport}}),
    {noreply, _State};
handle_cast({SessionId, timeout, NewTimeRef}, _State) ->
	Session = get_session(SessionId),
	case Session#session.timeRef of
		undefined -> ok;
		_ -> timer:cancel(Session#session.timeRef)
	end,
	ets:insert(?NickMap, {SessionId, Session#session{timeRef = NewTimeRef}}),
    {noreply, _State};
handle_cast({SessionId, end_connect}, _State) ->
	Session = get_session(SessionId),	
	ets:insert(?NickMap, {SessionId, Session#session{defined = undefined}}),
    {noreply, _State};
handle_cast({SessionId, endpoint, NewEndpoint}, _State) ->
	Session = get_session(SessionId),	
	ets:insert(?NickMap, {SessionId, Session#session{endpoint = NewEndpoint}}),	
    {noreply, _State};
%% handle_cast({SessionId, From, getEndpoint}, _State) ->
%% 	Session = get_session(SessionId),
%% 	From ! {endpoint, Session#session.endpoint},
%%     {noreply, _State};
handle_cast({SessionId, From, post, Message}, _State) ->
	Session = get_session(SessionId),
	{NewMessages, NewDefined} = handle_post_msg({From, Message}, Session, Session#session.transport),
	ets:insert(?NickMap, {SessionId, Session#session{messages = NewMessages, defined = NewDefined}}),
    {noreply, _State};
handle_cast(Msg, _State) ->
    {noreply, _State}.

handle_post_msg({_, Message}, Session, websocket) ->
	NewMessages = case Session#session.defined of
		undefined ->
			lager:debug("undefined~n", []),
			lists:merge(Session#session.messages, [Message]);
		Pid ->
			Pid ! {reply, Message},
			Session#session.messages
	end,
	{NewMessages, Session#session.defined};
handle_post_msg({_, Message}, Session, htmlfile) ->
	NewMessages = case Session#session.defined of
		undefined ->
			lists:merge(Session#session.messages, [Message]);
		Pid ->
			Pid ! {reply, Message},
			Session#session.messages
	end,
	{NewMessages, Session#session.defined};

handle_post_msg({_, Message}, Session, _) ->
	case Session#session.defined of
		undefined ->
			NewDefined = Session#session.defined,
			NewMessages = lists:merge([Message], Session#session.messages);
		Pid ->
			Pid ! {reply, Message},
			NewDefined = undefined,
			NewMessages = Session#session.messages
	end,
	{NewMessages, NewDefined}.

get_session(SessionId) ->
	Result = ets:lookup(?NickMap, SessionId),
	case Result of
		[{Key, Reply}] ->
			Reply;
		[] ->
			#session{}
	end.
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

