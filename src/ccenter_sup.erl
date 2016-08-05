-module(ccenter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Arg), {I, {I, start_link, Arg}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    case application:get_env(type) of
        {ok, center} ->
            {ok, {{one_for_one, 5, 10}, [?CHILD(ccenter_server, worker), ?CHILD(ccenter_connect_sup, supervisor)]}};
        {ok, normal} ->
            {ok, {{one_for_one, 5, 10}, [?CHILD(normal_connect_server, worker, [node()])]}}
    end.

