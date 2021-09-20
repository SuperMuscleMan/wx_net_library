%%%-------------------------------------------------------------------
%%% @author WeiMengHuan
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 三月 2021 21:43
%%%-------------------------------------------------------------------
-module(wx_net).
%%%=======================STATEMENT====================
-description("wx_net").
-copyright('').
-author("wmh, SuperMuscleMan@outlook.com").
%%%=======================EXPORT=======================
-export([set/1, get_cfg/1, get_all/0]).
%%%=======================INCLUDE======================

%%%=======================RECORD=======================

%%%=======================DEFINE=======================

%%%=================EXPORTED FUNCTIONS=================
set({{_, Tab}, Opt}) ->
	wx_cfg:set(?MODULE, Tab, Opt).

get_cfg(Tab) ->
	wx_cfg:get(?MODULE, Tab).

get_all()->
	wx_cfg:get(?MODULE).