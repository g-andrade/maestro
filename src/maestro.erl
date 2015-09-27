% vim: set expandtab softtabstop=2 shiftwidth=4:
-module(maestro).

-export([pick_pool/1,
         checkout/1, checkout/2, checkout/3,
         pool_checkout/1, pool_checkout/2, pool_checkout/3, pool_checkin/2,
         transaction/2, transaction/3,
         pool_transaction/2, pool_transaction/3,
         start/1, start/2,
         start_link/1, start_link/2,
         child_spec/3,
         stop/1,
         status/1, pool_status/1]).


-type maestro() :: maestro_serv:maestro().
-type maestro_ref() :: maestro_serv:maestro_ref().
-type pool_ref() :: maestro_serv:pool_ref().
-type start_ret() :: maestro_serv:start_ret().



-spec checkout(Maestro :: maestro()) -> {pool_ref(), pid()}.
checkout(Maestro) ->
    Pool = pick_pool(Maestro),
    {Pool, pool_checkout(Pool)}.

-spec checkout(Maestro :: maestro(), Block :: boolean()) -> {pool_ref(), pid()} | full.
checkout(Maestro, Block) ->
    Pool = pick_pool(Maestro),
    case pool_checkout(Pool, Block) of
        full -> full;
        WorkerPid -> {Pool, WorkerPid}
    end.

-spec checkout(Maestro :: maestro(), Block :: boolean(), Timeout :: timeout())
    -> {pool_ref(), pid()} | full.
checkout(Maestro, Block, Timeout) ->
    Pool = pick_pool(Maestro),
    case pool_checkout(Pool, Block, Timeout) of
        full -> full;
        WorkerPid -> {Pool, WorkerPid}
    end.

-spec transaction(Maestro :: maestro(), Fun :: fun((Worker :: pid()) -> any()))
    -> any().
transaction(Maestro, Fun) ->
    Pool = pick_pool(Maestro),
    pool_transaction(Pool, Fun).

-spec transaction(Maestro :: maestro(), Fun :: fun((Worker :: pid()) -> any()),
                  Timeout :: timeout()) -> any().
transaction(Maestro, Fun, Timeout) ->
    Pool = pick_pool(Maestro),
    pool_transaction(Pool, Fun, Timeout).

-spec start(MaestroArgs :: proplists:proplist())
    -> start_ret().
start(MaestroArgs) ->
    start(MaestroArgs, MaestroArgs).

-spec start(MaestroArgs :: proplists:proplist(),
            WorkerArgs:: proplists:proplist())
    -> start_ret().
start(MaestroArgs, WorkerArgs) ->
    maestro_serv:start_maestro(start, MaestroArgs, WorkerArgs).

-spec start_link(MaestroArgs :: proplists:proplist())
    -> start_ret().
start_link(MaestroArgs)  ->
    %% "for backwards compatability, pass the pool args as the worker args as well"
    start_link(MaestroArgs, MaestroArgs).

-spec start_link(MaestroArgs :: proplists:proplist(),
                 WorkerArgs:: proplists:proplist())
    -> start_ret().
start_link(MaestroArgs, WorkerArgs)  ->
    maestro_serv:start_maestro(start_link, MaestroArgs, WorkerArgs).

-spec child_spec(Id :: term(),
                 MaestroArgs :: proplists:proplist(),
                 WorkerArgs :: proplists:proplist())
    -> supervisor:child_spec().
child_spec(PoolId, MaestroArgs, WorkerArgs) ->
    {PoolId, {maestro_serv, start_maestro, [start_link, MaestroArgs, WorkerArgs]},
     permanent, 5000, worker, [maestro_serv]}.

-spec stop(MaestroRef :: maestro_ref()) -> ok.
stop(MaestroRef) ->
    maestro_serv:stop(MaestroRef).

-spec status(Maestro :: maestro()) -> [{atom(), integer(), integer(), integer()}].
status(Maestro) ->
    Pools = maestro_serv:all_pools(Maestro),
    [pool_status(Pool) || Pool <- Pools].


-spec pick_pool(Maestro :: maestro()) -> pool_ref().
pick_pool(Maestro) ->
    maestro_serv:pick_pool(Maestro).

-spec pool_checkout(Pool :: pool_ref()) -> pid().
pool_checkout(Pool) ->
    poolboy:checkout(Pool).

-spec pool_checkout(Pool :: pool_ref(), Block :: boolean()) -> pid() | full.
pool_checkout(Pool, Block) ->
    poolboy:checkout(Pool, Block).

-spec pool_checkout(Pool :: pool_ref(), Block :: boolean(), Timeout :: timeout())
    -> pool_ref() | full.
pool_checkout(Pool, Block, Timeout) ->
    poolboy:checkout(Pool, Block, Timeout).

-spec pool_checkin(Pool :: pool_ref(), Worker :: pid()) -> ok.
pool_checkin(Pool, Worker) when is_pid(Worker) ->
    poolboy:checkin(Pool, Worker).

-spec pool_transaction(Pool :: pool_ref(), Fun :: fun((Worker :: pid()) -> any()))
    -> any().
pool_transaction(Pool, Fun) ->
    poolboy:transaction(Pool, Fun).

-spec pool_transaction(Pool :: pool_ref(), Fun :: fun((Worker :: pid()) -> any()),
                       Timeout :: timeout()) -> any().
pool_transaction(Pool, Fun, Timeout) ->
    poolboy:transaction(Pool, Fun, Timeout).

-spec pool_status(Pool :: pool_ref()) -> {atom(), integer(), integer(), integer()}.
pool_status(Pool) ->
    poolboy:status(Pool).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
