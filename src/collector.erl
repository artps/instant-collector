-module(collector).

-export([start/0]).

ensure_started(App) ->
    case application:start(App) of
        {error, {already_started, App}} -> ok;
        _ -> ok
    end.

start() ->
    sync:go(),

    ensure_started(sockjs),
    ensure_started(cowboy),
    ensure_started(collector).
