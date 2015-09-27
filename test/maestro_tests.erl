% vim: set expandtab softtabstop=2 shiftwidth=4:
-module(maestro_tests).

-include_lib("eunit/include/eunit.hrl").

% based on poolboy's 'poolboy_tests'

maestro_test_() ->
    {foreach,
        fun() ->
            error_logger:tty(false)
        end,
        fun(_) ->
            case whereis(maestro_test) of
                undefined -> ok;
                Pid -> pool_call(Pid, stop)
            end,
            error_logger:tty(true)
        end,
        [{<<"Balanced pool picking">>, fun picking_is_balanced/0},
         {<<"Wrapping is functional">>, fun general_behaviour/0}
        ]
    }.


picking_is_balanced() ->
    random:seed(erlang:now()),
    PoolCount = 2 + random:uniform(100),
    {ok, Maestro} = new_maestro(start_link, PoolCount, 0, 0),
    SamplesCount = 100000,
    SamplesDict = lists:foldl(
        fun (_, Acc) ->
                PoolRef = maestro:pick_pool(Maestro),
                dict:update(PoolRef, fun (V) -> V + 1 end, 1, Acc)
        end,
        dict:new(), lists:seq(1, SamplesCount)),
    ok = maestro:stop(Maestro),
    {_, Samples} = lists:unzip( dict:to_list(SamplesDict)),
    StdDev = std_deviation(Samples),
    RelStdDev = StdDev / (SamplesCount / PoolCount),
    ?assert(RelStdDev =< 0.05).

general_behaviour() ->
    {ok, Maestro} = new_maestro(start2, 1, 1, 1),
    ?assertEqual(maestro:status(Maestro), [{ready, 1, 0, 0}]),

    {{poolboy, Pool}=PoolRef, Worker1} = maestro:checkout(Maestro),
    ?assertEqual(maestro:status(Maestro), [{overflow, 0, 0, 1}]),


    Worker2 = maestro:pool_checkout(PoolRef),
    ?assert(is_pid(Worker2)),
    ?assertEqual(maestro:status(Maestro), [{full, 0, 1, 2}]),
    ?assertEqual(maestro:pool_checkout(PoolRef, false), full),
    ?assertEqual(full, maestro:checkout(Maestro, false, 500)),
    ?assertEqual(maestro:checkout(Maestro, false), full),
    ?assertMatch({'EXIT', {timeout, _}}, catch maestro:checkout(Maestro, true, 0)),

    ?assertEqual(ok, maestro:pool_checkin(PoolRef, Worker1)),
    ?assertEqual(maestro:status(Maestro), [{overflow, 0, 0, 1}]),
    {{poolboy, Pool}, Worker3} = maestro:checkout(Maestro, true),
    ?assertEqual(ok, maestro:pool_checkin(PoolRef, Worker3)),
    {{poolboy, Pool}, Worker4} = maestro:checkout(Maestro, true, 100),
    ?assertEqual(ok, maestro:pool_checkin(PoolRef, Worker4)),

    ?assertEqual(ok, maestro:pool_checkin(PoolRef, Worker2)),
    ?assertEqual(maestro:status(Maestro), [{ready, 1, 0, 0}]),

    TrxFun = fun (Worker) -> gen_server:call(Worker, hello) end,
    ?assertEqual(ok, maestro:pool_transaction(PoolRef, TrxFun)),
    ?assertEqual(ok, maestro:pool_transaction(PoolRef, TrxFun, 1000)),
    ?assertEqual(ok, maestro:transaction(Maestro, TrxFun)),
    ?assertEqual(ok, maestro:transaction(Maestro, TrxFun, 1000)),

    ?assertEqual(maestro:pick_pool(Maestro), {poolboy, Pool}),
    exit(Pool, kill),
    timer:sleep(50),
    ?assertEqual(maestro:status(Maestro), [{ready, 1, 0, 0}]),
    ?assertNotEqual(maestro:pick_pool(Maestro), {poolboy, Pool}),

    {poolboy, Pool2} = maestro:pick_pool(Maestro),
    ?assertEqual(ok, maestro:stop(Maestro)),
    timer:sleep(50),
    ?assert(not is_process_alive(Pool2)),

    {pool_ref_id, {StartM, StartF, StartArgs}, _, _, _, [_|_]} = maestro:child_spec(
            pool_ref_id, [], []),
    ?assert(lists:member({StartF, length(StartArgs)},
                         proplists:get_value(exports, StartM:module_info()))),


    {ok, Maestro2} = new_maestro(start1, 1, 123, 456, undefined, false),
    ?assertEqual(maestro:status(Maestro2), [maestro:pool_status(maestro:pick_pool(Maestro2))]),
    {poolboy, Pool3} = maestro:pick_pool(Maestro2),
    ?assertEqual(ok, poolboy:stop(Pool3)),
    timer:sleep(50),
    ?assert(not is_process_alive(Pool3)),
    ?assertEqual(whereis(Maestro2), undefined).




new_maestro(StartType, PoolCount, Size, MaxOverflow) ->
    random:seed(erlang:now()),
    Name = list_to_atom("maestro_test_" ++ integer_to_list(random:uniform(1 bsl 26))),
    new_maestro(StartType, PoolCount, Size, MaxOverflow, Name).

new_maestro(StartType, PoolCount, Size, MaxOverflow, Name) ->
    new_maestro(StartType, PoolCount, Size, MaxOverflow, Name, true).

new_maestro(StartType, PoolCount, Size, MaxOverflow, Name, UseNamedPools) ->
    Args = [{worker_module, maestro_test_worker},
            {size, Size},
            {max_overflow, MaxOverflow},
            {pool_count, PoolCount},
            {pool_module, poolboy},
            {use_named_pools, UseNamedPools}]
           ++
           (case Name of
                undefined -> [];
                _ -> [{name, Name}]
            end),

    case StartType of
        start1 ->
            {ok, Pid} = maestro:start(Args),
            ?assert(is_pid(Pid));
        start2 ->
            {ok, Pid} = maestro:start(Args, []),
            ?assert(is_pid(Pid));
        start_link ->
            {ok, Pid} = maestro:start_link(Args),
            ?assert(is_pid(Pid))
    end,
    case Name of
        undefined -> {ok, maestro};
        _ -> {ok, Name}
    end.


%%%%%%%%%%%%%%%%%%
%% From 'http://fullof.bs/standard-deviation-root-mean-square-and-the-central-moments-statistics-in-erlang-part-3/'

std_deviation(Values) when is_list(Values) ->
    Mean = arithmetic_mean(Values),
    math:sqrt(arithmetic_mean([ (Val-Mean)*(Val-Mean) || Val <- Values ])).

arithmetic_mean(List) when is_list(List) ->
    lists:sum(List) / length(List).

%%%%%%%%%%%%%%%%%%

pool_call({poolboy, ServerRef}, Request) ->
    pool_call(ServerRef, Request);
pool_call(ServerRef, Request) ->
    gen_server:call(ServerRef, Request).
