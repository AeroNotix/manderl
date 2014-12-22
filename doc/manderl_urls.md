

# Module manderl_urls #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_tracking_domain-1">add_tracking_domain/1</a></td><td>
Add a tracking domain to your account.</td></tr><tr><td valign="top"><a href="#check_tracking_domain-1">check_tracking_domain/1</a></td><td>
Checks the CNAME settings for a tracking domain.</td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>
Get the 100 most clicked URLs.</td></tr><tr><td valign="top"><a href="#search-1">search/1</a></td><td>
Return the 100 most clicked URLs that match the search query given.</td></tr><tr><td valign="top"><a href="#time_series-1">time_series/1</a></td><td>
Return the recent history (hourly stats for the last 30 days) for a url.</td></tr><tr><td valign="top"><a href="#tracking_domains-0">tracking_domains/0</a></td><td>
Get the list of tracking domains set up for this account.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_tracking_domain-1"></a>

### add_tracking_domain/1 ###


<pre><code>
add_tracking_domain(Params::#{}) -&gt; #{}
</code></pre>
<br />



Add a tracking domain to your account


see [Mandrill documentation](https://mandrillapp.com/api/docs/urls.JSON.html#method=add-tracking-domain)
<a name="check_tracking_domain-1"></a>

### check_tracking_domain/1 ###


<pre><code>
check_tracking_domain(Params::#{}) -&gt; #{}
</code></pre>
<br />



Checks the CNAME settings for a tracking domain. The domain must have been added already with the add-tracking-domain call


see [Mandrill documentation](https://mandrillapp.com/api/docs/urls.JSON.html#method=check-tracking-domain)
<a name="list-0"></a>

### list/0 ###


<pre><code>
list() -&gt; #{}
</code></pre>
<br />



Get the 100 most clicked URLs


see [Mandrill documentation](https://mandrillapp.com/api/docs/urls.JSON.html#method=list)
<a name="search-1"></a>

### search/1 ###


<pre><code>
search(Params::#{}) -&gt; #{}
</code></pre>
<br />



Return the 100 most clicked URLs that match the search query given


see [Mandrill documentation](https://mandrillapp.com/api/docs/urls.JSON.html#method=search)
<a name="time_series-1"></a>

### time_series/1 ###


<pre><code>
time_series(Params::#{}) -&gt; #{}
</code></pre>
<br />



Return the recent history (hourly stats for the last 30 days) for a url


see [Mandrill documentation](https://mandrillapp.com/api/docs/urls.JSON.html#method=time-series)
<a name="tracking_domains-0"></a>

### tracking_domains/0 ###


<pre><code>
tracking_domains() -&gt; #{}
</code></pre>
<br />



Get the list of tracking domains set up for this account


see [Mandrill documentation](https://mandrillapp.com/api/docs/urls.JSON.html#method=tracking-domains)
