-module(collector_sensor_handler).

-export([init/3]).
-export([allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([handle_get/2, handle_put/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_http_rest}.

allowed_methods(Req, State) ->
    {['HEAD', 'PUT'], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, handle_put}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_get}
    ], Req, State}.


handle_get(Req, State) ->
    {SensorId, Req1} = cowboy_http_req:binding(sensor_id, Req),
    Body = jiffy:encode({[{sensor_id, SensorId}]}),
    {Body, Req1, State}.

handle_put(Req, State) ->
    {ok, Body, Req1} = cowboy_http_req:body(Req),
    {SensorId, Req2} = cowboy_http_req:binding(sensor_id, Req1),
    error_logger:info_msg("SensorId: ~p~n", [SensorId]),
    {true, Req2, State}.
