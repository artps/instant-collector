-module(collector_sockjs_handler).

-export([sockjs_init/2,
        sockjs_handle/3,
        sockjs_terminate/2]).

-record(state, {connection}).



sockjs_init(Connection, State) ->
    {ok, Pid} = collector_connection_sup:start_child(Connection),
    error_logger:info_msg("~n~n ============= PID ============= ~p~n~n~n", [Pid]),
    {ok, #state{connection=Pid}}.

sockjs_handle(Connection, Binary, State) ->
    {Data} = jiffy:decode(Binary),

    case proplists:get_value(<<"mode">>, Data) of
        <<"subscribe">> ->
            SensorId = proplists:get_value(<<"sensor_id">>, Data),
            collector_connection:subscribe(State#state.connection, SensorId);
        _ -> ok
    end,

    {ok, State}.

sockjs_terminate(Connection, State) ->
    {ok, State}.


send_message([], _) ->
    ok;
send_message([[Connection]|Connections], Data) ->
    Connection:send(Data),
    send_message(Connections, Data).

