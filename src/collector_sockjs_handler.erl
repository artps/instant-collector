-module(collector_sockjs_handler).

-export([sockjs_init/2,
        sockjs_handle/3,
        sockjs_terminate/2]).

sockjs_init(Connection, State) ->
  {ok, State}.

sockjs_handle(Connection, Data, State) ->
  {ok, State}.

sockjs_terminate(Connection, State) ->
  {ok, State}.
