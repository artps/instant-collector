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
    {ok, Connections} = collector_store:match({[{conn, '$1'}, {sensor_id, SensorId}]}),
    case Connections of
        [] -> ok;
        Connections -> send_message(Connections, Data)
    end.

send_message([], _) ->
    ok;
send_message([[Connection]|Connections], Data) ->
    Connection:send(Data),
    send_message(Connections, Data).

