%% @doc
%% @author Gregoire Lejeune <gl@finexkap.com>
%% @copyright 2014 Finexkap
%%
%% Manderl is a module for interacting with the Mandrill API
%%
%% ## Configuration
%% 
%% * `endpoint :: string()` : Mandrill endpoint (default: `"https://mandrillapp.com/api/1.0/"`)
%% * `api_key :: string()` : Mandrill API key
%% 
%% ## Usage
%%
%% All function take a map and return a map. Only the <code>key</code> parameter can be ommited, since it is added by the module.
%%
%% @end
-module(manderl_exports).

-export([
         info/1,
         list/0,
         rejects/1,
         whitelist/1,
         activity/1
        ]).

%% @doc
%% Returns information about an export job. If the export job's state is 'complete', the returned data will include a URL you can use to fetch the results. Every export job produces a zip archive, but the format of the archive is distinct for each job type. The api calls that initiate exports include more details about the output format for that job type.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/exports.JSON.html#method=info)
%% @end
-spec info(map()) -> map().
info(Params) ->
  manderl:call("/exports/info.json", Params).

%% @doc
%% Returns a list of your exports.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/exports.JSON.html#method=list)
%% @end
-spec list() -> map().
list() ->
  manderl:call("/exports/list.json", #{}).

%% @doc
%% Begins an export of your rejection blacklist. The blacklist will be exported to a zip archive containing a single file named rejects.csv that includes the following fields: email, reason, detail, created_at, expires_at, last_event_at, expires_at.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/exports.JSON.html#method=rejects)
%% @end
-spec rejects(map()) -> map().
rejects(Params) ->
  manderl:call("/exports/rejects.json", Params).

%% @doc
%% Begins an export of your rejection whitelist. The whitelist will be exported to a zip archive containing a single file named whitelist.csv that includes the following fields: email, detail, created_at.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/exports.JSON.html#method=whitelist)
%% @end
-spec whitelist(map()) -> map().
whitelist(Params) ->
  manderl:call("/exports/whitelist.json", Params).

%% @doc
%% Begins an export of your activity history. The activity will be exported to a zip archive containing a single file named activity.csv in the same format as you would be able to export from your account's activity view. It includes the following fields: Date, Email Address, Sender, Subject, Status, Tags, Opens, Clicks, Bounce Detail. If you have configured any custom metadata fields, they will be included in the exported data.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/exports.JSON.html#method=activity)
%% @end
-spec activity(map()) -> map().
activity(Params) ->
  manderl:call("/exports/activity.json", Params).

