%%%-------------------------------------------------------------------
%%% @author WeiMengHuan
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%	消息格式： 长度2byte + 内容
%%% @end
%%% Created : 12. 三月 2021 23:42
%%%-------------------------------------------------------------------
-module(wx_tcp_connect).
%%%=======================STATEMENT====================
-description("wx_tcp_connect").
-copyright('').
-author("wmh, SuperMuscleMan@outlook.com").
-behaviour(gen_server).

%% API
-export([start_link/0, start/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).
-define(Hibernate_TimeOut, 10000). %%休眠超时时间(毫秒)

-record(state, {src, tab_src, attr, ip, port, socket,
	handle_m, protocol_m, sub_bin}).

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
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start(Args) ->
	WordSize = erlang:system_info(wordsize),
	gen_server:start(?MODULE, Args,
		[{spawn_opt, [{min_heap_size, 64 * 1024 div WordSize}, {fullsweep_after, 128}]}]).

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
init({Src, TabSrc, Socket, HandleM, ProtocolM}) ->
	{ok, {Ip, Port}} = inet:peername(Socket),
	{ok, #state{src = Src, tab_src = TabSrc, attr = init_attr(Socket), ip = Ip,
		port = Port, socket = Socket, handle_m = HandleM,
		protocol_m = ProtocolM}, ?Hibernate_TimeOut}.

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
handle_info(a, #state{socket = Socket} = State) ->
	?SOUT(catch inet_db:lookup_socket(Socket)),
	?SOUT(catch inet_tcp:recv(Socket, 1)),
	?SOUT(catch prim_inet:getstatus(Socket)),
	?SOUT(catch inet:getstat(Socket)),
	?SOUT(catch prim_inet:getopts(Socket, [active])),
	?SOUT(catch gen_tcp:send(Socket, "aaa")),
	{noreply, State};
handle_info({tcp, Socket, Bin}, #state{src = Src, tab_src = TabSrc, attr = Attr,
	handle_m = HandleM, protocol_m = ProtoM, sub_bin = SubBin} = State) ->
	Bin1 =
		case is_binary(SubBin) of
			false -> Bin;
			_ ->
				<<SubBin/binary, Bin/binary>>
		end,
	R1 =
		case ProtoM:decode(Bin1) of
			{Name, none, Term} ->
				?DEBUG({no_port_module, Term}),
				?ERR({no_port_module, [{proto_name, Name}, {proto_data, Term}]}),
				{stop, decode_failed};
			{Name, Mfs, Term} ->
				?DEBUG([Name, Mfs, Term]),
				HandleM:handle(Mfs, Src, TabSrc, Attr, Term, Socket, ProtoM, State);
			{Name, Mfs, Term, SubBin1} ->
				?DEBUG([Name, Mfs, Term]),
				R = HandleM:handle(Mfs, Src, TabSrc, Attr, Term, Socket, ProtoM, State),
				{R, SubBin1}
		end,
	case R1 of
		ok ->
			{noreply, State, ?Hibernate_TimeOut};
		{ok, NowSunBin} ->
			{noreply, State#state{sub_bin = NowSunBin}, ?Hibernate_TimeOut};
		{attr, NowAttr} ->
			{noreply, State#state{attr = NowAttr}, ?Hibernate_TimeOut};
		{{attr, NowAttr}, NowSubBin} ->
			{noreply, State#state{attr = NowAttr, sub_bin = NowSubBin},
				?Hibernate_TimeOut};
		{stop, Reason} ->
			gen_tcp:close(Socket),
			{stop, Reason, State};
		{{stop, Reason}, _} ->
			gen_tcp:close(Socket),
			{stop, Reason, State}
	end;
handle_info(timeout, State) ->
	{noreply, State, hibernate};
handle_info(_Info, #state{handle_m = HandleM, src = Src, tab_src = TabSrc,
	attr = Attr, socket = Socket} = State) ->
	?ERR(_Info),
	HandleM:net_close(Src, TabSrc, Attr, Socket),
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
	?ERR(_Reason),
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
%% 初始化Attr参数
init_attr(Socket) ->
	Tree = gb_trees:empty(),
	gb_trees:insert(socket, Socket, Tree).
	