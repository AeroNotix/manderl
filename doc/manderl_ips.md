

# Module manderl_ips #
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2014 Finexkap

Manderl is a module for interacting with the Mandrill API

## Configuration

* `endpoint :: string()` : Mandrill endpoint (default: `"https://mandrillapp.com/api/1.0/"`)
* `api_key :: string()` : Mandrill API key

## Usage

All function take a map and return a map. Only the <code>key</code> parameter can be ommited, since it is added by the module.


__Authors:__ Gregoire Lejeune ([`gl@finexkap.com`](mailto:gl@finexkap.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cancel_warmup-1">cancel_warmup/1</a></td><td>
Cancels the warmup process for a dedicated IP.</td></tr><tr><td valign="top"><a href="#check_custom_dns-1">check_custom_dns/1</a></td><td>
Tests whether a domain name is valid for use as the custom reverse DNS for a dedicated IP.</td></tr><tr><td valign="top"><a href="#create_pool-1">create_pool/1</a></td><td>
Creates a pool and returns it.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>
Deletes a dedicated IP.</td></tr><tr><td valign="top"><a href="#delete_pool-1">delete_pool/1</a></td><td>
Deletes a pool.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>
Retrieves information about a single dedicated ip.</td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>
Lists your dedicated IPs.</td></tr><tr><td valign="top"><a href="#list_pools-0">list_pools/0</a></td><td>
Lists your dedicated IP pools.</td></tr><tr><td valign="top"><a href="#pool_info-1">pool_info/1</a></td><td>
Describes a single dedicated IP pool.</td></tr><tr><td valign="top"><a href="#provision-1">provision/1</a></td><td>
Requests an additional dedicated IP for your account.</td></tr><tr><td valign="top"><a href="#set_custom_dns-1">set_custom_dns/1</a></td><td>
Configures the custom DNS name for a dedicated IP.</td></tr><tr><td valign="top"><a href="#set_pool-1">set_pool/1</a></td><td>
Moves a dedicated IP to a different pool.</td></tr><tr><td valign="top"><a href="#start_warmup-1">start_warmup/1</a></td><td>
Begins the warmup process for a dedicated IP.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cancel_warmup-1"></a>

### cancel_warmup/1 ###


<pre><code>
cancel_warmup(Params::#{}) -&gt; #{}
</code></pre>
<br />



Cancels the warmup process for a dedicated IP.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=cancel-warmup)
<a name="check_custom_dns-1"></a>

### check_custom_dns/1 ###


<pre><code>
check_custom_dns(Params::#{}) -&gt; #{}
</code></pre>
<br />



Tests whether a domain name is valid for use as the custom reverse DNS for a dedicated IP.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=check-custom-dns)
<a name="create_pool-1"></a>

### create_pool/1 ###


<pre><code>
create_pool(Params::#{}) -&gt; #{}
</code></pre>
<br />



Creates a pool and returns it. If a pool already exists with this name, no action will be performed.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=create-pool)
<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Params::#{}) -&gt; #{}
</code></pre>
<br />



Deletes a dedicated IP. This is permanent and cannot be undone.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=delete)
<a name="delete_pool-1"></a>

### delete_pool/1 ###


<pre><code>
delete_pool(Params::#{}) -&gt; #{}
</code></pre>
<br />



Deletes a pool. A pool must be empty before you can delete it, and you cannot delete your default pool.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=delete-pool)
<a name="info-1"></a>

### info/1 ###


<pre><code>
info(Params::#{}) -&gt; #{}
</code></pre>
<br />



Retrieves information about a single dedicated ip.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=info)
<a name="list-0"></a>

### list/0 ###


<pre><code>
list() -&gt; #{}
</code></pre>
<br />



Lists your dedicated IPs.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=list)
<a name="list_pools-0"></a>

### list_pools/0 ###


<pre><code>
list_pools() -&gt; #{}
</code></pre>
<br />



Lists your dedicated IP pools.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=list-pools)
<a name="pool_info-1"></a>

### pool_info/1 ###


<pre><code>
pool_info(Params::#{}) -&gt; #{}
</code></pre>
<br />



Describes a single dedicated IP pool.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=pool-info)
<a name="provision-1"></a>

### provision/1 ###


<pre><code>
provision(Params::#{}) -&gt; #{}
</code></pre>
<br />



Requests an additional dedicated IP for your account. Accounts may have one outstanding request at any time, and provisioning requests are processed within 24 hours.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=provision)
<a name="set_custom_dns-1"></a>

### set_custom_dns/1 ###


<pre><code>
set_custom_dns(Params::#{}) -&gt; #{}
</code></pre>
<br />



Configures the custom DNS name for a dedicated IP.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=set-custom-dns)
<a name="set_pool-1"></a>

### set_pool/1 ###


<pre><code>
set_pool(Params::#{}) -&gt; #{}
</code></pre>
<br />



Moves a dedicated IP to a different pool.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=set-pool)
<a name="start_warmup-1"></a>

### start_warmup/1 ###


<pre><code>
start_warmup(Params::#{}) -&gt; #{}
</code></pre>
<br />



Begins the warmup process for a dedicated IP. During the warmup process, Mandrill will gradually increase the percentage of your mail that is sent over the warming-up IP, over a period of roughly 30 days. The rest of your mail will be sent over shared IPs or other dedicated IPs in the same pool.


see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=start-warmup)
