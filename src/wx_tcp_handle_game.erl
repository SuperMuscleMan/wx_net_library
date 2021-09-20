%%%-------------------------------------------------------------------
%%% @author WeiMengHuan
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 4月 2021 23:16
%%%-------------------------------------------------------------------
-module(wx_tcp_handle_game).
%%%=======================STATEMENT====================
-description("wx_tcp_handle_game").
-copyright('').
-author("wmh, SuperMuscleMan@outlook.com").
%%%=======================EXPORT=======================
-export([handle/8, net_close/4]).
%%%=======================INCLUDE======================

-include_lib("wx_log_library/include/wx_log.hrl").

%%%=======================RECORD=======================

%%%=======================DEFINE=======================
-define(ISALTER_Y, 1).
-define(ISALTER_N, 0).
%%%=================EXPORTED FUNCTIONS=================
handle(Mfs, Src, TabSrc, Attr, Term, Socket, ProtoM, State) ->
	try
		handle_mf(Mfs, Src, TabSrc, Attr, Term, Socket, ProtoM, ?ISALTER_N)
	catch
		E1:E2:E3 ->
			?ERR({E1, E2, E3, State})
	end.

net_close(_Src, _TabSrc, _Attr, _Socket)->
	ok.
%==========================DEFINE=======================
handle_mf([{M, F} | T], Src, TabSrc, Attr, Term, Socket, ProtoM, IsAlter) ->
	case M:F(Src, TabSrc, Attr, Term) of
		{reply, R} ->
			gen_tcp:send(Socket, ProtoM:encode(R)),
			handle_mf(T, Src, TabSrc, Attr, Term, Socket, ProtoM, IsAlter);
		{reply, R, Attr1} ->
			gen_tcp:send(Socket, ProtoM:encode(R)),
			handle_mf(T, Src, TabSrc, Attr1, Term, Socket, ProtoM, ?ISALTER_Y);
		noreply ->
			handle_mf(T, Src,TabSrc,  Attr, Term, Socket, ProtoM, IsAlter);
		{stop, _Reason} = Stop->
			Stop
	end;
handle_mf([], _, _, Attr, _, _, _, IsAlter) ->
	case IsAlter of
		?ISALTER_Y ->
			{attr, Attr};
		_->
			ok
	end.