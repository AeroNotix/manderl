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
-module(manderl_rejects).

-export([
         add/1,
         list/1,
         delete/1
        ]).

%% @doc
%% Adds an email to your email rejection blacklist. Addresses that you add manually will never expire and there is no reputation penalty for removing them from your blacklist. Attempting to blacklist an address that has been whitelisted will have no effect.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/rejects.JSON.html#method=add)
%% @end
-spec add(map()) -> map().
add(Params) ->
  manderl:call("/rejects/add.json", Params).

%% @doc
%% Retrieves your email rejection blacklist. You can provide an email address to limit the results. Returns up to 1000 results. By default, entries that have expired are excluded from the results; set include_expired to true to include them.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/rejects.JSON.html#method=list)
%% @end
-spec list(map()) -> map().
list(Params) ->
  manderl:call("/rejects/list.json", Params).

%% @doc
%% Deletes an email rejection. There is no limit to how many rejections you can remove from your blacklist, but keep in mind that each deletion has an affect on your reputation. 
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/rejects.JSON.html#method=delete)
%% @end
-spec delete(map()) -> map().
delete(Params) ->
  manderl:call("/rejects/delete.json", Params).

