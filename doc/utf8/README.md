

# maestro #

Copyright (c) 2015 Guilherme Andrade

__Version:__ 1.0.0

__Authors:__ Guilherme Andrade ([`g@gandrade.net`](mailto:g@gandrade.net)).

`maestro`: a pool of pools for when single pool managers become a bottleneck.
---------


### <a name="Why?">Why?</a> ###


Big worker pools for short-lived I/O tasks (e.g. database access) can easily overrun a single `poolboy` manager due to too many check-in / check-out activity.


### <a name="How?">How?</a> ###


Take the naïve approach and launch multiple pools; pick them at random when checking out. Having `maestro` be aware of each pool workload would be nice but it would increase complexity and lower performance without any significant advantages when all the pools are already under a similar load pattern.


### <a name="How_do_I_use_it?">How do I use it?</a> ###


```erlang

MaestroName = many_pools,
Conf = [% maestro options
        {name, MaestroName},
        {pool_module, poolboy},
        {pool_count, 3},
        {use_named_pools, false},

        % poolboy options
        {worker_module, fabulous_worker},
        {size, 100},
        {max_overflow, 50}],

{ok, _} = maestro:start(Conf),

{SomePool, Worker1} = maestro:checkout(MaestroName),
thing_done = gen_server:call(Worker1, do_your_thing),
ok = maestro:pool_checkin(SomePool, Worker1),

also_done = maestro:transaction(
        MaestroName,
        fun (Worker) -> gen_server:call(Worker, do_your_other_thing) end),

ok = maestro:stop(MaestroName).

```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="maestro.md" class="module">maestro</a></td></tr>
<tr><td><a href="maestro_pool_sup.md" class="module">maestro_pool_sup</a></td></tr>
<tr><td><a href="maestro_serv.md" class="module">maestro_serv</a></td></tr></table>

