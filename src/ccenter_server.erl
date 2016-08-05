%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 中央节点注册进程，
%%% 1. 监听所有的节点连接
%%% 2. 启动节点连接的维护进程
%%% @end
%%% Created : 01. 八月 2016 11:08
%%%-------------------------------------------------------------------
-module(ccenter_server).
-author("calvin").
-include("otp.hrl").
-include("ccenter.hrl").
-include("common.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0,
    register/2,
    register_group/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

register_group(Key, PID) ->
    ?GENSERVER_CALL(?MODULE, {register_group, Key, PID}).

register(NodeName, PID) ->
    ?GENSERVER_CALL(?MODULE, {register, NodeName, PID}).

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
init([]) ->
    net_kernel:monitor_nodes(true, [node_type, all]),
    erlang:process_flag(trap_exit, true),
    {ok, #state{}}.

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
handle_call(Request, _From, State) ->
    case ?HANDLE_CALL(Request, State) of
        {Ok, NewState} when erlang:is_record(NewState, state) ->
            {reply, Ok, NewState};
        Ok ->
            {reply, Ok, State}
    end.


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
handle_cast(Request, State) ->
    ?HANDLE_CASE(Request, State),
    {noreply, State}.

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

handle_info({nodeup, Node}, State) ->
    ?INFO("Node = ~w  up", [Node]),
    {noreply, State};
handle_info({nodedown, Node}, State) ->
    ?INFO("Node = ~w  down", [Node]),
    del_game(Node),
    {noreply, State};
handle_info(Info, State) ->
    ?HANDLE_INFO(Info, State),
    {noreply, State}.

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

do_handle({register_group, Key, PID}, _State) ->
    case ets:lookup(?ETS_CENTER_DATA, group_pids) of
        [{group_pids, Values}] -> ets:insert(?ETS_CENTER_DATA, {group_pids, [{Key, PID} | Values]});
        _ -> ets:insert(?ETS_CENTER_DATA, {group_pids, [{Key, PID}]})
    end,
    ok;
%% @return {ok, PID}
do_handle({register, NodeName, GamePID}, _State) ->
    add_game(NodeName, GamePID),
    erlang:monitor_node(NodeName, true),
    ?INFO("node =~w  register", [NodeName]),
    {ok, ConnectPID} = ccenter_connect_sup:start_child(NodeName),
    ccenter_connect_server:update(ConnectPID, NodeName, GamePID),
    {ok, ConnectPID};

do_handle(Msg, State) ->
    io:format("received an unkonow message  = ~w, State = ~w", [Msg, State]).

add_game(NodeName, PID) ->
    ets:insert(?ETS_CENTER_DATA, {NodeName, PID}),
    case ets:lookup(?ETS_CENTER_DATA, ?CENTER_GAMES) of
        [{?CENTER_GAMES, Nodes}] ->
            ets:insert(?ETS_CENTER_DATA, {?CENTER_GAMES, [NodeName | Nodes]});
        [] ->
            ets:insert(?ETS_CENTER_DATA, {?CENTER_GAMES, [NodeName]})
    end.

del_game(NodeName) ->
    ets:delete(?ETS_CENTER_DATA, NodeName),
    case ets:lookup(?ETS_CENTER_DATA, ?CENTER_GAMES) of
        [{?CENTER_GAMES, Nodes}] ->
            ets:insert(?ETS_CENTER_DATA, {?CENTER_GAMES, lists:delete(NodeName, Nodes)});
        _ ->
            ?ERROR("Internal error")
    end.
