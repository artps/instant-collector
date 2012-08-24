-module(collector_sockjs_handler).

-export([sockjs_init/2,
        sockjs_handle/3,
        sockjs_terminate/2]).

-export([broadcast/2]).


sockjs_init(Connection, State) ->
    {ok, State}.

sockjs_handle(Connection, Binary, State) ->
    {Data} = jiffy:decode(Binary),

    case proplists:get_value(<<"mode">>, Data) of
        <<"subscribe">> ->
            subscribe(Connection, proplists:get_value(<<"sensor_id">>, Data));
        <<"unsubscribe">> ->
            unsubscribe(Connection, proplists:get_value(<<"sensor_id">>, Data));
        _ -> ok
    end,

    {ok, State}.

sockjs_terminate(Connection, State) ->
    {ok, State}.


subscribe(Connection, SensorId) ->
    collector_store:insert([
        { conn, Connection },
        { sensor_id, SensorId }
    ]).

unsubscribe(Connection, SensorId) ->
    collector_store:delete([
        { conn, Connection },
        { sensor_id, SensorId }
    ]).


broadcast(SensorId, Data) when is_binary(SensorId) ->
    broadcast(list_to_integer(binary_to_list(SensorId)), Data);
broadcast(SensorId, Data) ->
    collector_store:foldl(fun(Item, _Acc) ->
        case Item of
            {[ {conn, Conn}, {sensor_id, Num} ]} when Num =:= SensorId ->
                Conn:send(Data);
            _ -> ok
        end
    end).
