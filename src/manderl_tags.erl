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
-module(manderl_tags).

-export([
         list/0,
         delete/1,
         info/1,
         time_series/1,
         all_time_series/0
        ]).

%% @doc
%% Return all of the user-defined tag information
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/tags.JSON.html#method=list)
%% @end
-spec list() -> map().
list() ->
  manderl:call("/tags/list.json", #{}).

%% @doc
%% Deletes a tag permanently. Deleting a tag removes the tag from any messages that have been sent, and also deletes the tag's stats. There is no way to undo this operation, so use it carefully.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/tags.JSON.html#method=delete)
%% @end
-spec delete(map()) -> map().
delete(Params) ->
  manderl:call("/tags/delete.json", Params).

%% @doc
%% Return more detailed information about a single tag, including aggregates of recent stats
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/tags.JSON.html#method=info)
%% @end
-spec info(map()) -> map().
info(Params) ->
  manderl:call("/tags/info.json", Params).

%% @doc
%% Return the recent history (hourly stats for the last 30 days) for a tag
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/tags.JSON.html#method=time-series)
%% @end
-spec time_series(map()) -> map().
time_series(Params) ->
  manderl:call("/tags/time-series.json", Params).

%% @doc
%% Return the recent history (hourly stats for the last 30 days) for all tags
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/tags.JSON.html#method=all-time-series)
%% @end
-spec all_time_series() -> map().
all_time_series() ->
  manderl:call("/tags/all-time-series.json", #{}).

