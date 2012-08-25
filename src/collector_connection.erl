-module(collector_connection).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([subscribe/2,
         broadcast/2]).


-record(state, {connection}).

-define(GROUP, broadcast).


subscribe(Pid, SensorId) when is_integer(SensorId) ->
    subscribe(Pid, integer_to_list(SensorId));
subscribe(Pid, SensorId) ->
    gen_server:call(Pid, {subscribe, SensorId}).


broadcast(SensorId, Data) when is_binary(SensorId) ->
    broadcast(binary_to_list(SensorId), Data);
broadcast(SensorId, Data) ->
    %gproc:lookup_pids({p, l, {?GROUP, SensorId}}).
    gproc:bcast({p, l, {?GROUP, SensorId}}, {broadcast, Data}).
    %<ok.

start_link(Connection) ->
    gen_server:start_link(?MODULE, [Connection], []).


init([Connection]) ->
    {ok, #state{connection=Connection}}.

handle_call({subscribe, SensorId}, _From, State) ->
    gproc:reg({p, l, {?GROUP, SensorId}}),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({broadcast, Data}, #state{connection = Connection} = State) ->
    Connection:send(Data),
    {noreply, State};

handle_info(Info, State) ->
    error_logger:info_msg("~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
