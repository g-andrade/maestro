% vim: set expandtab softtabstop=2 shiftwidth=4:
-module(maestro_serv).
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([pick_pool/1,
         all_pools/1,
         stop/1,
         start_maestro/3,
         start_managed_pool/4]).

-export_type([maestro/0,
              maestro_ref/0,
              pool_ref/0,
              start_ret/0]).


-record(pool, {
        pid :: pid(),
        module :: module() }).

-record(monitored_pool, {
        pid :: pid(),
        monitor :: reference() }).

-record(state, {
        pool_count = 1 :: non_neg_integer(),
        pool_args = [] :: proplists:proplist(),
        pool_module = poolboy :: module(),
        monitored_pools = [] :: [#monitored_pool{}],
        name = maestro :: atom(),
        use_named_pools = false :: boolean() }).

-type maestro() :: atom().
-type maestro_ref() :: maestro() | pid().
-type pool_ref() :: {module(), pid()}.
-type start_ret() :: {'ok', pid()} | 'ignore' | {'error', term()}.


-spec start_maestro(StartFun :: start | start_link,
                    MaestroArgs :: proplists:proplist(),
                    WorkerArgs :: proplists:proplist()) -> start_ret().
start_maestro(StartFun, MaestroArgs, WorkerArgs) ->
    Name = case proplists:get_value(name, MaestroArgs) of
        undefined -> (#state{})#state.name;
        ExplicitName when is_atom(ExplicitName) ->
            ExplicitName
    end,
    gen_server:StartFun({local, Name}, maestro_serv, {MaestroArgs, WorkerArgs}, []).

-spec register_pool(MaestroRef :: maestro_ref(), PoolPid :: pid()) -> ok.
register_pool(MaestroRef, PoolPid) ->
    gen_server:cast(MaestroRef, {register_pool, PoolPid}).

-spec pick_pool(Maestro :: maestro()) -> PoolRef :: pool_ref().
pick_pool(Maestro) ->
    Pools = ets:tab2list(Maestro),
    PoolIdx = 1 + erlang:phash2({self(), os:timestamp()}, length(Pools)),
    #pool{ pid=PoolPid, module=PoolModule } = lists:nth(PoolIdx, Pools),
    true = is_pid(PoolPid) orelse exit({'no pools available', Maestro}),
    {PoolModule, PoolPid}.

-spec all_pools(Maestro :: maestro()) -> [pool_ref()].
all_pools(Maestro) ->
    [{Module, Pid} || #pool{ pid=Pid, module=Module } <- ets:tab2list(Maestro)].

-spec stop(MaestroRef :: maestro_ref()) -> ok.
stop(MaestroRef) ->
    ok = gen_server:call(MaestroRef, stop).

-spec start_managed_pool(MaestroPid::pid(), M::module(), F::atom(), A::undefined | [term()])
        -> start_ret().
start_managed_pool(MaestroPid, M, F, A) ->
    CoalescedA = case A of
        undefined -> [];
        _ -> A
    end,
    case apply(M, F, CoalescedA) of
        {ok, Pid}=Result ->
            register_pool(MaestroPid, Pid),
            Result;
        Other ->
            Other
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init({PoolArgs, WorkerArgs}) ->
    init(PoolArgs, WorkerArgs, #state{}).

init([{name, Name} | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State#state{name = Name});
init([{pool_count, PoolCount} | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State#state{pool_count = PoolCount});
init([{pool_module, PoolModule} | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State#state{pool_module = PoolModule});
init([{use_named_pools, UseNamedPools} | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State#state{use_named_pools = UseNamedPools});
init([OtherArg | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State#state{pool_args = [OtherArg | State#state.pool_args]});
init([], WorkerArgs, #state{} = State) ->
    #state{pool_count=PoolCount,
           pool_args=PoolArgs,
           name=Name,
           use_named_pools=UseNamedPools,
           pool_module=PoolModule }=State,

    Name = ets:new(Name, [set, protected, named_table,
                          {keypos, #pool.pid},
                          {read_concurrency, true}]),

    NamedPoolsSetting = case UseNamedPools of
        true  -> {named_pools, Name};
        false -> anonymous_pools
    end,
    Children = pool_child_specs(self(), NamedPoolsSetting, PoolCount,
                                PoolModule, PoolArgs, WorkerArgs),
    {ok, _PoolSupervisorPid} = maestro_pool_sup:start_link(Children),
    {ok, State}.


handle_call(stop, _From, #state{}=State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, #state{}=State) ->
    {noreply, State}.


handle_cast({register_pool, PoolPid}, #state{ name=Name, pool_module=PoolModule }=State)
        when is_pid(PoolPid), PoolPid /= self()
->
    Pool = #pool{pid = PoolPid,
                 module = PoolModule},
    true = ets:insert(Name, Pool),
    MonitoredPool = #monitored_pool{pid = PoolPid,
                                    monitor = monitor(process, PoolPid)},
    {noreply, State#state{ monitored_pools = [MonitoredPool | State#state.monitored_pools] }};

handle_cast(_Msg, #state{}=State) ->
    {noreply, State}.


handle_info({'DOWN', Reference, process, Pid, Reason}, #state{}=State) ->
    case lists:keytake(Reference, #monitored_pool.monitor, State#state.monitored_pools) of
        false ->
            {noreply, State};
        {value, #monitored_pool{ pid=Pid }, NewMonitoredPools} ->
            % Propagate clean shutdowns.
            case Reason of
                normal        -> {stop, normal, State};
                shutdown      -> {stop, normal, State};
                {shutdown, _} -> {stop, normal, State};
                _ ->
                    true = ets:delete(State#state.name, Pid),
                    {noreply, State#state{ monitored_pools=NewMonitoredPools }}
            end
    end;

handle_info(_Info, #state{}=State) ->
    {noreply, State}.


terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec pool_child_specs(MaestroPid :: pid(),
                       PoolNameSetting :: {use_named_pools, BaseName::atom()} | anonymous_pools,
                       PoolCount :: pos_integer(),
                       PoolModule :: module(), BasePoolArgs :: proplists:proplist(), WorkerArgs :: proplists:proplist())
        -> [supervisor:child_spec()].
pool_child_specs(MaestroPid, PoolNameSetting, PoolCount, PoolModule, BasePoolArgs, WorkerArgs) ->
    lists:map(
        fun (Index) ->
                ChildId = list_to_atom("pool_" ++ integer_to_list(Index)),
                PoolArgs = case PoolNameSetting of
                    anonymous_pools -> lists:keydelete(name, 1, BasePoolArgs);
                    {named_pools, BaseName} ->
                        PoolName = list_to_atom(atom_to_list(BaseName)
                                                ++ "_pool"  ++ integer_to_list(Index)),
                        lists:keystore(name, 1, BasePoolArgs, {name, {local, PoolName}})
                end,

                % Let's wrap the underlying start function in our own
                % so that we can generically notify the maestro.
                ChildSpec = PoolModule:child_spec(ChildId, PoolArgs, WorkerArgs),
                {Id, {M, F, A}, Restart, Shutdown, Type, Modules }=ChildSpec,
                {Id, {?MODULE, start_managed_pool, [MaestroPid, M, F, A]},
                 Restart, Shutdown, Type, Modules}
        end,
        lists:seq(1, PoolCount)).
