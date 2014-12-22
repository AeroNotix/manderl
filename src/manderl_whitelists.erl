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
-module(manderl_whitelists).

-export([
         add/1,
         list/1,
         delete/1
        ]).

%% @doc
%% Adds an email to your email rejection whitelist. If the address is currently on your blacklist, that blacklist entry will be removed automatically.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/whitelists.JSON.html#method=add)
%% @end
-spec add(map()) -> map().
add(Params) ->
  manderl:call("/whitelists/add.json", Params).

%% @doc
%% Retrieves your email rejection whitelist. You can provide an email address or search prefix to limit the results. Returns up to 1000 results.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/whitelists.JSON.html#method=list)
%% @end
-spec list(map()) -> map().
list(Params) ->
  manderl:call("/whitelists/list.json", Params).

%% @doc
%% Removes an email address from the whitelist.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/whitelists.JSON.html#method=delete)
%% @end
-spec delete(map()) -> map().
delete(Params) ->
  manderl:call("/whitelists/delete.json", Params).

