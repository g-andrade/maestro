% vim: set expandtab softtabstop=2 shiftwidth=4:
-module(maestro).
-export([pick_pool/1,
         checkout/1, checkout/2, checkout/3,
         transaction/2, transaction/3,
         start/1, start/2,
         start_link/1, start_link/2,
         child_spec/3,
         stop/1, status/1,
         pool_checkout/1, pool_checkout/2, pool_checkout/3,
         pool_checkin/2,
         pool_transaction/2, pool_transaction/3,
         pool_status/1]).

-export_type([maestro/0,
              maestro_ref/0,
              pool_ref/0]).

-ignore_xref([{pick_pool, 1},
              {checkout, 1}, {checkout, 2}, {checkout, 3},
              {transaction, 2}, {transaction, 3},
              {start, 1}, {start, 2},
              {start_link, 1}, {start_link, 2},
              {child_spec, 3},
              {stop, 1}, {status, 1},
              {pool_checkout, 1}, {pool_checkout, 2}, {pool_checkout, 3},
              {pool_checkin, 2},
              {pool_transaction, 2}, {pool_transaction, 3},
              {pool_status, 1}]).

-type maestro() :: maestro_serv:maestro().
-type maestro_ref() :: maestro_serv:maestro_ref().
-type pool_ref() :: maestro_serv:pool_ref().
-type start_ret() :: maestro_serv:start_ret().


% poolboy-inspired interface.


-spec checkout(Maestro :: maestro()) -> {pool_ref(), pid()}.
checkout(Maestro) ->
    PoolRef = pick_pool(Maestro),
    {PoolRef, pool_checkout(PoolRef)}.

-spec checkout(Maestro :: maestro(), Block :: boolean()) -> {pool_ref(), pid()} | full.
checkout(Maestro, Block) ->
    PoolRef = pick_pool(Maestro),
    case pool_checkout(PoolRef, Block) of
        full -> full;
        WorkerPid -> {PoolRef, WorkerPid}
    end.

-spec checkout(Maestro :: maestro(), Block :: boolean(), Timeout :: timeout())
    -> {pool_ref(), pid()} | full.
checkout(Maestro, Block, Timeout) ->
    PoolRef = pick_pool(Maestro),
    case pool_checkout(PoolRef, Block, Timeout) of
        full -> full;
        WorkerPid -> {PoolRef, WorkerPid}
    end.

-spec transaction(Maestro :: maestro(), Fun :: fun((Worker :: pid()) -> any()))
    -> any().
transaction(Maestro, Fun) ->
    PoolRef = pick_pool(Maestro),
    pool_transaction(PoolRef, Fun).

-spec transaction(Maestro :: maestro(), Fun :: fun((Worker :: pid()) -> any()),
                  Timeout :: timeout()) -> any().
transaction(Maestro, Fun, Timeout) ->
    PoolRef = pick_pool(Maestro),
    pool_transaction(PoolRef, Fun, Timeout).

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
    %% "for backwards compatability, pass the pool args as the worker args as well" (poolboy)
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
child_spec(PoolRefId, MaestroArgs, WorkerArgs) ->
    {PoolRefId, {maestro_serv, start_maestro, [start_link, MaestroArgs, WorkerArgs]},
     permanent, 5000, worker, [maestro_serv]}.

-spec stop(MaestroRef :: maestro_ref()) -> ok.
stop(MaestroRef) ->
    maestro_serv:stop(MaestroRef).

-spec status(Maestro :: maestro()) -> [{atom(), integer(), integer(), integer()}].
status(Maestro) ->
    PoolRefs = maestro_serv:all_pools(Maestro),
    [pool_status(PoolRef) || PoolRef <- PoolRefs].

-spec pick_pool(Maestro :: maestro()) -> pool_ref().
pick_pool(Maestro) ->
    maestro_serv:pick_pool(Maestro).


-spec pool_checkout(PoolRef :: pool_ref()) -> pid().
pool_checkout({Module, Pool}) ->
    Module:checkout(Pool).

-spec pool_checkout(PoolRef :: pool_ref(), Block :: boolean()) -> pid() | full.
pool_checkout({Module, Pool}, Block) ->
    Module:checkout(Pool, Block).

-spec pool_checkout(PoolRef :: pool_ref(), Block :: boolean(), Timeout :: timeout())
    -> pool_ref() | full.
pool_checkout({Module, Pool}, Block, Timeout) ->
    Module:checkout(Pool, Block, Timeout).

-spec pool_checkin(PoolRef :: pool_ref(), Worker :: pid()) -> ok.
pool_checkin({Module, Pool}, Worker) when is_pid(Worker) ->
    Module:checkin(Pool, Worker).

-spec pool_transaction(PoolRef :: pool_ref(), Fun :: fun((Worker :: pid()) -> any()))
    -> any().
pool_transaction({Module, Pool}, Fun) ->
    Module:transaction(Pool, Fun).

-spec pool_transaction(PoolRef :: pool_ref(), Fun :: fun((Worker :: pid()) -> any()),
                       Timeout :: timeout()) -> any().
pool_transaction({Module, Pool}, Fun, Timeout) ->
    Module:transaction(Pool, Fun, Timeout).

-spec pool_status(PoolRef :: pool_ref()) -> {atom(), integer(), integer(), integer()}.
pool_status({Module, Pool}) ->
    Module:status(Pool).
