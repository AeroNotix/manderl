

# Module manderl_exports #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#activity-1">activity/1</a></td><td>
Begins an export of your activity history.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>
Returns information about an export job.</td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>
Returns a list of your exports.</td></tr><tr><td valign="top"><a href="#rejects-1">rejects/1</a></td><td>
Begins an export of your rejection blacklist.</td></tr><tr><td valign="top"><a href="#whitelist-1">whitelist/1</a></td><td>
Begins an export of your rejection whitelist.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="activity-1"></a>

### activity/1 ###


<pre><code>
activity(Params::#{}) -&gt; #{}
</code></pre>
<br />



Begins an export of your activity history. The activity will be exported to a zip archive containing a single file named activity.csv in the same format as you would be able to export from your account's activity view. It includes the following fields: Date, Email Address, Sender, Subject, Status, Tags, Opens, Clicks, Bounce Detail. If you have configured any custom metadata fields, they will be included in the exported data.


see [Mandrill documentation](https://mandrillapp.com/api/docs/exports.JSON.html#method=activity)
<a name="info-1"></a>

### info/1 ###


<pre><code>
info(Params::#{}) -&gt; #{}
</code></pre>
<br />



Returns information about an export job. If the export job's state is 'complete', the returned data will include a URL you can use to fetch the results. Every export job produces a zip archive, but the format of the archive is distinct for each job type. The api calls that initiate exports include more details about the output format for that job type.


see [Mandrill documentation](https://mandrillapp.com/api/docs/exports.JSON.html#method=info)
<a name="list-0"></a>

### list/0 ###


<pre><code>
list() -&gt; #{}
</code></pre>
<br />



Returns a list of your exports.


see [Mandrill documentation](https://mandrillapp.com/api/docs/exports.JSON.html#method=list)
<a name="rejects-1"></a>

### rejects/1 ###


<pre><code>
rejects(Params::#{}) -&gt; #{}
</code></pre>
<br />



Begins an export of your rejection blacklist. The blacklist will be exported to a zip archive containing a single file named rejects.csv that includes the following fields: email, reason, detail, created_at, expires_at, last_event_at, expires_at.


see [Mandrill documentation](https://mandrillapp.com/api/docs/exports.JSON.html#method=rejects)
<a name="whitelist-1"></a>

### whitelist/1 ###


<pre><code>
whitelist(Params::#{}) -&gt; #{}
</code></pre>
<br />



Begins an export of your rejection whitelist. The whitelist will be exported to a zip archive containing a single file named whitelist.csv that includes the following fields: email, detail, created_at.


see [Mandrill documentation](https://mandrillapp.com/api/docs/exports.JSON.html#method=whitelist)
