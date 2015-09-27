

# Module maestro_pool_sup #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`supervisor`](supervisor.md).

<a name="types"></a>

## Data Types ##




### <a name="type-startlink_err">startlink_err()</a> ###



<pre><code>
startlink_err() = {already_started, pid()} | {shutdown, term()} | term()
</code></pre>





### <a name="type-startlink_ret">startlink_ret()</a> ###



<pre><code>
startlink_ret() = {ok, pid()} | ignore | {error, <a href="#type-startlink_err">startlink_err()</a>}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###

`init(Pools) -> any()`


<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(Pools::[<a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>]) -&gt; <a href="#type-startlink_ret">startlink_ret()</a>
</code></pre>

<br></br>



