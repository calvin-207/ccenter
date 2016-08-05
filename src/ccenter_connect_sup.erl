%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%     连接进程的监控者
%%% @end
%%% Created : 01. 八月 2016 11:09
%%%-------------------------------------------------------------------
-module(ccenter_connect_sup).
-author("calvin").

-behaviour(supervisor).

%% API
-export([start_link/0,
    start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_child(NodeName) ->
    case erlang:whereis(NodeName) of
        undefined ->
            supervisor:start_child(?MODULE, [NodeName]);
        PID ->
            {ok, PID}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |ignore |{error, Reason :: term()}).
init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(ccenter_connect_server, worker)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
