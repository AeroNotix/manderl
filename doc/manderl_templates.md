

# Module manderl_templates #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-1">add/1</a></td><td>
Add a new template.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>
Delete a template.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>
Get the information for an existing template.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td>
Return a list of all the templates available to this user.</td></tr><tr><td valign="top"><a href="#publish-1">publish/1</a></td><td>
Publish the content for the template.</td></tr><tr><td valign="top"><a href="#render-1">render/1</a></td><td>
Inject content and optionally merge fields into a template, returning the HTML that results.</td></tr><tr><td valign="top"><a href="#time_series-1">time_series/1</a></td><td>
Return the recent history (hourly stats for the last 30 days) for a template.</td></tr><tr><td valign="top"><a href="#update-1">update/1</a></td><td>
Update the code for an existing template.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-1"></a>

### add/1 ###


<pre><code>
add(Params::#{}) -&gt; #{}
</code></pre>
<br />



Add a new template


see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=add)
<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Params::#{}) -&gt; #{}
</code></pre>
<br />



Delete a template


see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=delete)
<a name="info-1"></a>

### info/1 ###


<pre><code>
info(Params::#{}) -&gt; #{}
</code></pre>
<br />



Get the information for an existing template


see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=info)
<a name="list-1"></a>

### list/1 ###


<pre><code>
list(Params::#{}) -&gt; #{}
</code></pre>
<br />



Return a list of all the templates available to this user


see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=list)
<a name="publish-1"></a>

### publish/1 ###


<pre><code>
publish(Params::#{}) -&gt; #{}
</code></pre>
<br />



Publish the content for the template. Any new messages sent using this template will start using the content that was previously in draft.


see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=publish)
<a name="render-1"></a>

### render/1 ###


<pre><code>
render(Params::#{}) -&gt; #{}
</code></pre>
<br />



Inject content and optionally merge fields into a template, returning the HTML that results


see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=render)
<a name="time_series-1"></a>

### time_series/1 ###


<pre><code>
time_series(Params::#{}) -&gt; #{}
</code></pre>
<br />



Return the recent history (hourly stats for the last 30 days) for a template


see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=time-series)
<a name="update-1"></a>

### update/1 ###


<pre><code>
update(Params::#{}) -&gt; #{}
</code></pre>
<br />



Update the code for an existing template. If null is provided for any fields, the values will remain unchanged.


see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=update)
