

# Module maestro #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-maestro">maestro()</a> ###



<pre><code>
maestro() = <a href="maestro_serv.md#type-maestro">maestro_serv:maestro()</a>
</code></pre>





### <a name="type-maestro_ref">maestro_ref()</a> ###



<pre><code>
maestro_ref() = <a href="maestro_serv.md#type-maestro_ref">maestro_serv:maestro_ref()</a>
</code></pre>





### <a name="type-pool_ref">pool_ref()</a> ###



<pre><code>
pool_ref() = <a href="maestro_serv.md#type-pool_ref">maestro_serv:pool_ref()</a>
</code></pre>





### <a name="type-start_ret">start_ret()</a> ###



<pre><code>
start_ret() = <a href="maestro_serv.md#type-start_ret">maestro_serv:start_ret()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#checkout-1">checkout/1</a></td><td>Pick a pool and check-out one worker.</td></tr><tr><td valign="top"><a href="#checkout-2">checkout/2</a></td><td>Pick a pool and check-out one worker; optional block.</td></tr><tr><td valign="top"><a href="#checkout-3">checkout/3</a></td><td>Pick a pool and check-out one worker; optional block and check-out timeout.</td></tr><tr><td valign="top"><a href="#child_spec-3">child_spec/3</a></td><td></td></tr><tr><td valign="top"><a href="#pick_pool-1">pick_pool/1</a></td><td>Throw some dice.</td></tr><tr><td valign="top"><a href="#pool_checkin-2">pool_checkin/2</a></td><td>Check in a particular worker into an arbitrary pool.</td></tr><tr><td valign="top"><a href="#pool_checkout-1">pool_checkout/1</a></td><td>Check-out one worker from an arbitrary pool.</td></tr><tr><td valign="top"><a href="#pool_checkout-2">pool_checkout/2</a></td><td>Check-out one worker from an arbitrary pool; optional block.</td></tr><tr><td valign="top"><a href="#pool_checkout-3">pool_checkout/3</a></td><td>Check-out one worker from an arbitrary pool; optional block and check-out timeout.</td></tr><tr><td valign="top"><a href="#pool_status-1">pool_status/1</a></td><td>Run a single function in-between a check-out and a check-in; optional check-out timeout.</td></tr><tr><td valign="top"><a href="#pool_transaction-2">pool_transaction/2</a></td><td></td></tr><tr><td valign="top"><a href="#pool_transaction-3">pool_transaction/3</a></td><td>Run a single function in-between a check-out and a check-in.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td></td></tr><tr><td valign="top"><a href="#status-1">status/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#transaction-2">transaction/2</a></td><td>Pick a pool and run a single function in-between a check-out and a check-in.</td></tr><tr><td valign="top"><a href="#transaction-3">transaction/3</a></td><td>Pick a pool and run a single function in-between a check-out and a check-in; optional check-out timeout.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="checkout-1"></a>

### checkout/1 ###


<pre><code>
checkout(Maestro::<a href="#type-maestro">maestro()</a>) -&gt; {<a href="#type-pool_ref">pool_ref()</a>, pid()}
</code></pre>

<br></br>


Pick a pool and check-out one worker.

<a name="checkout-2"></a>

### checkout/2 ###


<pre><code>
checkout(Maestro::<a href="#type-maestro">maestro()</a>, Block::boolean()) -&gt; {<a href="#type-pool_ref">pool_ref()</a>, pid()} | full
</code></pre>

<br></br>


Pick a pool and check-out one worker; optional block.

<a name="checkout-3"></a>

### checkout/3 ###


<pre><code>
checkout(Maestro::<a href="#type-maestro">maestro()</a>, Block::boolean(), Timeout::timeout()) -&gt; {<a href="#type-pool_ref">pool_ref()</a>, pid()} | full
</code></pre>

<br></br>


Pick a pool and check-out one worker; optional block and check-out timeout.

<a name="child_spec-3"></a>

### child_spec/3 ###


<pre><code>
child_spec(Id::term(), MaestroArgs::<a href="proplists.md#type-proplist">proplists:proplist()</a>, WorkerArgs::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>
</code></pre>

<br></br>



<a name="pick_pool-1"></a>

### pick_pool/1 ###


<pre><code>
pick_pool(Maestro::<a href="#type-maestro">maestro()</a>) -&gt; <a href="#type-pool_ref">pool_ref()</a>
</code></pre>

<br></br>


Throw some dice.
<a name="pool_checkin-2"></a>

### pool_checkin/2 ###


<pre><code>
pool_checkin(PoolRef::<a href="#type-pool_ref">pool_ref()</a>, Worker::pid()) -&gt; ok
</code></pre>

<br></br>


Check in a particular worker into an arbitrary pool.

<a name="pool_checkout-1"></a>

### pool_checkout/1 ###


<pre><code>
pool_checkout(PoolRef::<a href="#type-pool_ref">pool_ref()</a>) -&gt; pid()
</code></pre>

<br></br>


Check-out one worker from an arbitrary pool.

<a name="pool_checkout-2"></a>

### pool_checkout/2 ###


<pre><code>
pool_checkout(PoolRef::<a href="#type-pool_ref">pool_ref()</a>, Block::boolean()) -&gt; pid() | full
</code></pre>

<br></br>


Check-out one worker from an arbitrary pool; optional block.

<a name="pool_checkout-3"></a>

### pool_checkout/3 ###


<pre><code>
pool_checkout(PoolRef::<a href="#type-pool_ref">pool_ref()</a>, Block::boolean(), Timeout::timeout()) -&gt; <a href="#type-pool_ref">pool_ref()</a> | full
</code></pre>

<br></br>


Check-out one worker from an arbitrary pool; optional block and check-out timeout.

<a name="pool_status-1"></a>

### pool_status/1 ###


<pre><code>
pool_status(PoolRef::<a href="#type-pool_ref">pool_ref()</a>) -&gt; {atom(), integer(), integer(), integer()}
</code></pre>

<br></br>


Run a single function in-between a check-out and a check-in; optional check-out timeout.

<a name="pool_transaction-2"></a>

### pool_transaction/2 ###


<pre><code>
pool_transaction(PoolRef::<a href="#type-pool_ref">pool_ref()</a>, Fun::fun((Worker::pid()) -&gt; any())) -&gt; any()
</code></pre>

<br></br>



<a name="pool_transaction-3"></a>

### pool_transaction/3 ###


<pre><code>
pool_transaction(PoolRef::<a href="#type-pool_ref">pool_ref()</a>, Fun::fun((Worker::pid()) -&gt; any()), Timeout::timeout()) -&gt; any()
</code></pre>

<br></br>


Run a single function in-between a check-out and a check-in.

<a name="start-1"></a>

### start/1 ###


<pre><code>
start(MaestroArgs::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; <a href="#type-start_ret">start_ret()</a>
</code></pre>

<br></br>



<a name="start-2"></a>

### start/2 ###


<pre><code>
start(MaestroArgs::<a href="proplists.md#type-proplist">proplists:proplist()</a>, WorkerArgs::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; <a href="#type-start_ret">start_ret()</a>
</code></pre>

<br></br>



<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(MaestroArgs::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; <a href="#type-start_ret">start_ret()</a>
</code></pre>

<br></br>



<a name="start_link-2"></a>

### start_link/2 ###


<pre><code>
start_link(MaestroArgs::<a href="proplists.md#type-proplist">proplists:proplist()</a>, WorkerArgs::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; <a href="#type-start_ret">start_ret()</a>
</code></pre>

<br></br>



<a name="status-1"></a>

### status/1 ###


<pre><code>
status(Maestro::<a href="#type-maestro">maestro()</a>) -&gt; [{atom(), integer(), integer(), integer()}]
</code></pre>

<br></br>



<a name="stop-1"></a>

### stop/1 ###


<pre><code>
stop(MaestroRef::<a href="#type-maestro_ref">maestro_ref()</a>) -&gt; ok
</code></pre>

<br></br>



<a name="transaction-2"></a>

### transaction/2 ###


<pre><code>
transaction(Maestro::<a href="#type-maestro">maestro()</a>, Fun::fun((Worker::pid()) -&gt; any())) -&gt; any()
</code></pre>

<br></br>


Pick a pool and run a single function in-between a check-out and a check-in.

<a name="transaction-3"></a>

### transaction/3 ###


<pre><code>
transaction(Maestro::<a href="#type-maestro">maestro()</a>, Fun::fun((Worker::pid()) -&gt; any()), Timeout::timeout()) -&gt; any()
</code></pre>

<br></br>


Pick a pool and run a single function in-between a check-out and a check-in; optional check-out timeout.

