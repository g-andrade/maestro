

# Module maestro_serv #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-maestro">maestro()</a> ###



<pre><code>
maestro() = atom()
</code></pre>





### <a name="type-maestro_ref">maestro_ref()</a> ###



<pre><code>
maestro_ref() = <a href="#type-maestro">maestro()</a> | pid()
</code></pre>





### <a name="type-pool_ref">pool_ref()</a> ###



<pre><code>
pool_ref() = {module(), pid()}
</code></pre>





### <a name="type-start_ret">start_ret()</a> ###



<pre><code>
start_ret() = {ok, pid()} | ignore | {error, term()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_pools-1">all_pools/1</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#pick_pool-1">pick_pool/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_maestro-3">start_maestro/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_managed_pool-4">start_managed_pool/4</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_pools-1"></a>

### all_pools/1 ###


<pre><code>
all_pools(Maestro::<a href="#type-maestro">maestro()</a>) -&gt; [<a href="#type-pool_ref">pool_ref()</a>]
</code></pre>

<br></br>



<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`


<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`


<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`


<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`


<a name="pick_pool-1"></a>

### pick_pool/1 ###


<pre><code>
pick_pool(Maestro::<a href="#type-maestro">maestro()</a>) -&gt; PoolRef::<a href="#type-pool_ref">pool_ref()</a>
</code></pre>

<br></br>



<a name="start_maestro-3"></a>

### start_maestro/3 ###


<pre><code>
start_maestro(StartFun::start | start_link, MaestroArgs::<a href="proplists.md#type-proplist">proplists:proplist()</a>, WorkerArgs::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; <a href="#type-start_ret">start_ret()</a>
</code></pre>

<br></br>



<a name="start_managed_pool-4"></a>

### start_managed_pool/4 ###


<pre><code>
start_managed_pool(MaestroPid::pid(), M::module(), F::atom(), A::undefined | [term()]) -&gt; <a href="#type-start_ret">start_ret()</a>
</code></pre>

<br></br>



<a name="stop-1"></a>

### stop/1 ###


<pre><code>
stop(MaestroRef::<a href="#type-maestro_ref">maestro_ref()</a>) -&gt; ok
</code></pre>

<br></br>



<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


