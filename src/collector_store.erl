-module(collector_store).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([insert/1, delete/1, foldl/1]).

-record(state, {}).

-define(TABLE, subscription).


insert(Data) ->
    gen_server:call(?MODULE, {insert, Data}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

foldl(Callback) ->
    gen_server:call(?MODULE, {foldl, Callback}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    maybe_create(),
    {ok, #state{}}.


handle_call({insert, Data}, _From, State) ->
    true = ets:insert(?TABLE, {Data}),
    {reply, ok, State};

handle_call({delete, Key}, _From, State) ->
    true = ets:delete_object(?TABLE, {Key}),
    {reply, ok, State};

handle_call({foldl, Callback}, _From, State) ->
    ets:foldl(Callback, [], ?TABLE),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


maybe_create() ->
    case ets:info(?TABLE, memory) of
        undefined ->
            ets:new(?TABLE, [public, named_table]);
        _Any ->
            ok
    end.
