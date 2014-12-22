

# Module manderl_metadata #
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
Add a new custom metadata field to be indexed for the account.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>
Delete an existing custom metadata field.</td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>
Get the list of custom metadata fields indexed for the account.</td></tr><tr><td valign="top"><a href="#update-1">update/1</a></td><td>
Update an existing custom metadata field.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-1"></a>

### add/1 ###


<pre><code>
add(Params::#{}) -&gt; #{}
</code></pre>
<br />



Add a new custom metadata field to be indexed for the account.


see [Mandrill documentation](https://mandrillapp.com/api/docs/metadata.JSON.html#method=add)
<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Params::#{}) -&gt; #{}
</code></pre>
<br />



Delete an existing custom metadata field. Deletion isn't instataneous, and /metadata/list will continue to return the field until the asynchronous deletion process is complete.


see [Mandrill documentation](https://mandrillapp.com/api/docs/metadata.JSON.html#method=delete)
<a name="list-0"></a>

### list/0 ###


<pre><code>
list() -&gt; #{}
</code></pre>
<br />



Get the list of custom metadata fields indexed for the account.


see [Mandrill documentation](https://mandrillapp.com/api/docs/metadata.JSON.html#method=list)
<a name="update-1"></a>

### update/1 ###


<pre><code>
update(Params::#{}) -&gt; #{}
</code></pre>
<br />



Update an existing custom metadata field.


see [Mandrill documentation](https://mandrillapp.com/api/docs/metadata.JSON.html#method=update)
