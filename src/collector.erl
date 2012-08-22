-module(collector).

-export([start/0]).

start() ->
    application:start(cowboy),
	application:start(collector).
