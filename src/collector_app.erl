-module(collector_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(TABLE, broadcast_table).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ensure_ets_exists(),
    collector_sup:start_link().

stop(_State) ->
    ok.

ensure_ets_exists() ->
    case ets:info(?TABLE, memory) of
        undefined ->
            ets:new(?TABLE, [public, named_table]);
        _Any ->
            ok
    end.
