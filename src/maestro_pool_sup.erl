% vim: set expandtab softtabstop=2 shiftwidth=4:
-module(maestro_pool_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

% copied from supervisor
-type startlink_err() :: {already_started, pid()} | {shutdown, term()} | term().
-type startlink_ret() :: {ok, pid()} | ignore | {error, startlink_err()}.


-spec start_link(Pools :: [supervisor:child_spec()]) -> startlink_ret().
start_link(Pools) ->
    supervisor:start_link(?MODULE, Pools).


init(Pools) ->
    {ok, {{one_for_one, 5, 10}, Pools}}.
