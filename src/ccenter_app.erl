%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%      中央机连接服务
%%% @end
%%% Created : 01. 八月 2016 11:09
%%%-------------------------------------------------------------------
-module(ccenter_app).
-include("ccenter.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ets:new(?ETS_CENTER_DATA, [set, named_table, public, {read_concurrency, true}]),
    ccenter_sup:start_link().


stop(_State) ->
    ok.
