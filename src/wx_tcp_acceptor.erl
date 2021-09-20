%%%-------------------------------------------------------------------
%%% @author WeiMengHuan
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 三月 2021 23:27
%%%-------------------------------------------------------------------
-module(wx_tcp_acceptor).
%%%=======================STATEMENT====================
-description("wx_tcp_accept").
-copyright('').
-author("wmh, SuperMuscleMan@outlook.com").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).
-define(Hibernate_TimeOut, 10000). %%休眠超时时间(毫秒)

-record(state, {src, tab_src, listen, opt, ref, handle_m, protocol_m}).

-include_lib("wx_log_library/include/wx_log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args::term()) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init({Src, TabSrc, Listen, Opt, HandleM, ProtocolM}) ->
	{ok, Ref} = prim_inet:async_accept(Listen, -1),
	
	{ok, #state{src = Src, tab_src = TabSrc, listen = Listen, opt = Opt,
		ref = Ref, handle_m = HandleM, protocol_m = ProtocolM}, ?Hibernate_TimeOut}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
	{reply, ok, State, ?Hibernate_TimeOut}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
	{noreply, State, ?Hibernate_TimeOut}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info({inet_async, Listen, Ref, {ok, Socket}}, #state{ref = Ref, opt = Opt,
	src = Src, tab_src = TabSrc, handle_m = HandleM, protocol_m = ProtoM} = State) ->
	{Ip, Port} = prim_inet:peername(Socket),
	?SOUT([{ip, Ip}, {port, Port}]),
	true = inet_db:register_socket(Socket, inet_db:tcp_module()),
	{ok, Pid} = wx_tcp_connect:start({Src, TabSrc, Socket, HandleM, ProtoM}),
	ok = inet:tcp_controlling_process(Socket, Pid),
	ok = inet:setopts(Socket, Opt),
	?SOUT([{socketOpt, prim_inet:getopt(Socket, active)}]),
	{ok, Ref1} = prim_inet:async_accept(Listen, -1),
	?SOUT({self, self(),newRef, Ref1, newListen, Listen}),
	{noreply, State#state{ref = Ref1}, ?Hibernate_TimeOut};
handle_info(timeout, State)->
	{noreply, State, hibernate};
handle_info(_Info, State) ->
	?SOUT(_Info),
	{noreply, State, ?Hibernate_TimeOut}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
		Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
