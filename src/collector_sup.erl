-module(collector_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
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
    {ok, Port} = application:get_env(collector, port),
    {ok, Workers} = application:get_env(collector, workers),

    SockjsState = sockjs_handler:init_state(
        <<"/channel">>, collector_sockjs_handler, state, []
    ),

    Dispatch = [
        {'_', [
            {[<<"channel">>, '...'], sockjs_cowboy_handler, SockjsState},
            {[<<"sensors">>, sensor_id], collector_sensor_handler, []}
        ]}
    ],
    ChildSpec = cowboy:child_spec(collector_http, Workers,
        cowboy_tcp_transport, [{port, Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

    {ok, { {one_for_one, 5, 10}, [ChildSpec]} }.

