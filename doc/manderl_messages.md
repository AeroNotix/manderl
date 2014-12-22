

# Module manderl_messages #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cancel_scheduled-1">cancel_scheduled/1</a></td><td>
Cancels a scheduled email.</td></tr><tr><td valign="top"><a href="#content-1">content/1</a></td><td>
Get the full content of a recently sent message.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>
Get the information for a single recently sent message.</td></tr><tr><td valign="top"><a href="#list_scheduled-1">list_scheduled/1</a></td><td>
Queries your scheduled emails by sender or recipient, or both.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>
Parse the full MIME document for an email message, returning the content of the message broken into its constituent pieces.</td></tr><tr><td valign="top"><a href="#reschedule-1">reschedule/1</a></td><td>
Cancels a scheduled email.</td></tr><tr><td valign="top"><a href="#search-1">search/1</a></td><td>
Search recently sent messages and optionally narrow by date range, tags, senders, and API keys.</td></tr><tr><td valign="top"><a href="#search_time_series-1">search_time_series/1</a></td><td>
Search the content of recently sent messages and return the aggregated hourly stats for matching messages.</td></tr><tr><td valign="top"><a href="#send-1">send/1</a></td><td>
Send a new transactional message through Mandrill.</td></tr><tr><td valign="top"><a href="#send_raw-1">send_raw/1</a></td><td>
Take a raw MIME document for a message, and send it exactly as if it were sent through Mandrill's SMTP servers.</td></tr><tr><td valign="top"><a href="#send_template-1">send_template/1</a></td><td>
Send a new transactional message through Mandrill using a template.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cancel_scheduled-1"></a>

### cancel_scheduled/1 ###


<pre><code>
cancel_scheduled(Params::#{}) -&gt; #{}
</code></pre>
<br />



Cancels a scheduled email.


see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=cancel-scheduled)
<a name="content-1"></a>

### content/1 ###


<pre><code>
content(Params::#{}) -&gt; #{}
</code></pre>
<br />



Get the full content of a recently sent message


see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=content)
<a name="info-1"></a>

### info/1 ###


<pre><code>
info(Params::#{}) -&gt; #{}
</code></pre>
<br />



Get the information for a single recently sent message


see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=info)
<a name="list_scheduled-1"></a>

### list_scheduled/1 ###


<pre><code>
list_scheduled(Params::#{}) -&gt; #{}
</code></pre>
<br />



Queries your scheduled emails by sender or recipient, or both.


see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=list-scheduled)
<a name="parse-1"></a>

### parse/1 ###


<pre><code>
parse(Params::#{}) -&gt; #{}
</code></pre>
<br />



Parse the full MIME document for an email message, returning the content of the message broken into its constituent pieces


see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=parse)
<a name="reschedule-1"></a>

### reschedule/1 ###


<pre><code>
reschedule(Params::#{}) -&gt; #{}
</code></pre>
<br />



Cancels a scheduled email.


see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=reschedule)
<a name="search-1"></a>

### search/1 ###


<pre><code>
search(Params::#{}) -&gt; #{}
</code></pre>
<br />



Search recently sent messages and optionally narrow by date range, tags, senders, and API keys. If no date range is specified, results within the last 7 days are returned. This method may be called up to 20 times per minute. If you need the data more often, you can use /messages/info.json to get the information for a single message, or webhooks to push activity to your own application for querying.


see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=search)
<a name="search_time_series-1"></a>

### search_time_series/1 ###


<pre><code>
search_time_series(Params::#{}) -&gt; #{}
</code></pre>
<br />



Search the content of recently sent messages and return the aggregated hourly stats for matching messages


see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=search-time-series)
<a name="send-1"></a>

### send/1 ###


<pre><code>
send(Params::#{}) -&gt; #{}
</code></pre>
<br />



Send a new transactional message through Mandrill


see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=send)
<a name="send_raw-1"></a>

### send_raw/1 ###


<pre><code>
send_raw(Params::#{}) -&gt; #{}
</code></pre>
<br />



Take a raw MIME document for a message, and send it exactly as if it were sent through Mandrill's SMTP servers


see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=send-raw)
<a name="send_template-1"></a>

### send_template/1 ###


<pre><code>
send_template(Params::#{}) -&gt; #{}
</code></pre>
<br />



Send a new transactional message through Mandrill using a template


see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=send-template)
