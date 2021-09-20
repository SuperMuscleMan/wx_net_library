%%%-------------------------------------------------------------------
%%% @author WeiMengHuan
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 三月 2021 10:34
%%%-------------------------------------------------------------------
-module(wx_tcp_protocol_game).
%%%=======================STATEMENT====================
-description("wx_tcp_protocol").
-copyright('').
-author("wmh, SuperMuscleMan@outlook.com").
%%%=======================EXPORT=======================
-export([decode/1, encode/1]).
%%%=======================INCLUDE======================

-include_lib("wx_log_library/include/wx_log.hrl").

%%%=======================RECORD=======================

%%%=======================DEFINE=======================
-define(Length_Cmd, 16). %% 消息id长度 （单位：比特
%%%=================EXPORTED FUNCTIONS=================

decode(<<Cmd:?Length_Cmd, Sub/binary>>) ->
	case wx_cfg:get(proto_cfg_num, Cmd) of
		none ->
			?ERR({no_cmd, {bin_cmd, Cmd}}),
			error({no_cmd, 'bin_cmd:', Cmd});
		Name ->
			Mfs = wx_cfg:get(wx_port, Name),
			Term = 'pb_GameProto':decode_msg(Sub, Name),
			{Name, Mfs, Term}
	end.

encode(R) ->
	Name = element(1, R),
	Cmd = wx_cfg:get(proto_cfg_name, Name),
	Bin = 'pb_GameProto':encode_msg(R),
	<<Cmd:?Length_Cmd, Bin/binary>>.

%==========================DEFINE=======================