%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%      提供给外部使用的接口
%%% @end
%%% Created : 01. 八月 2016 13:57
%%%-------------------------------------------------------------------
-module(ccenter).
-author("calvin").
-include("ccenter.hrl").
-include("common.hrl").
-include("otp.hrl").
%% API
-export([
    send/2,
    call/2,
    cast/2,
    broadcast/1,
    get_games/0,
    register_group/2]).

%% 给所有节点广播消息
%% 发给各个跟节点连接的ccenter_connect_server，由其在发送给各个节点。
broadcast(_Msg) ->
    ok.

%% @doc  连如中央机的所有游戏服
%% @doc  [NameName]
get_games() ->
    case ets:lookup(?ETS_CENTER_DATA, ?CENTER_GAMES) of
        [{?CENTER_GAMES, Games}] -> Games;
        [] -> []
    end.

send(NodeName, Message) ->
    case ets:lookup(?ETS_CENTER_DATA, NodeName) of
        [{NodeName, GamePID}] ->
            erlang:send(GamePID, Message);
        _ ->
            ?ERROR("error,try to send message to an unconnect node=~w", [NodeName]),
            erlang:send(NodeName, Message)
    end.

call(NodeName, Message) ->
    case ets:lookup(?ETS_CENTER_DATA, NodeName) of
        [{NodeName, GamePID}] ->
            ?GENSERVER_CALL(GamePID, Message);
        _ ->
            ?ERROR("error,try to send message to an unconnect node=~w", [NodeName]),
            gen_server:call(NodeName, Message)
    end.

cast(NodeName, Message) ->
    case ets:lookup(?ETS_CENTER_DATA, NodeName) of
        [{NodeName, GamePID}] ->
            ?GENSERVER_CAST(GamePID, Message);
        _ ->
            ?ERROR("error,try to send message to an unconnect node=~w", [NodeName]),
            gen_server:cast(NodeName, Message)
    end.

%% @doc 把改进程同步到各个游戏节点
-spec register_group(Key::atom(), PID::pid()) -> term().
register_group(Key, PID) ->
    ccenter_server:register_group(Key, PID).