-module(collector_stream_handler).

-export([init/4, stream/3, info/3, terminate/2]).
-export([broadcast/2]).

-define(GROUP, broadcast).


broadcast(SensorId, Data) when is_binary(SensorId) ->
    broadcast(binary_to_list(SensorId), Data);
broadcast(SensorId, Data) ->
    gproc:bcast({p, l, {?GROUP, SensorId}}, {broadcast, Data}).

init(_Transport, Req, _Opts, _Active) ->
	{ok, Req, undefined}.

stream(Bin, Req, State) ->
    {Data} = jiffy:decode(Bin),
    case proplists:get_value(<<"mode">>, Data) of
        <<"subscribe">> ->
            SensorId = integer_to_list(proplists:get_value(<<"sensor_id">>, Data)),
            gproc:reg({p, l, {?GROUP, SensorId}});
        _ -> ok
    end,

	{ok, Req, State}.

info({broadcast, Data}, Req, State) ->
    {reply, Data, Req, State};

info(Info, Req, State) ->
	{ok, Req, State}.

terminate(_Req, _State) ->
	ok.
