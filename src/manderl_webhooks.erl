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
-module(manderl_webhooks).

-export([
         list/0,
         add/1,
         info/1,
         update/1,
         delete/1
        ]).

%% @doc
%% Get the list of all webhooks defined on the account
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/webhooks.JSON.html#method=list)
%% @end
-spec list() -> map().
list() ->
  manderl:call("/webhooks/list.json", #{}).

%% @doc
%% Add a new webhook
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/webhooks.JSON.html#method=add)
%% @end
-spec add(map()) -> map().
add(Params) ->
  manderl:call("/webhooks/add.json", Params).

%% @doc
%% Given the ID of an existing webhook, return the data about it
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/webhooks.JSON.html#method=info)
%% @end
-spec info(map()) -> map().
info(Params) ->
  manderl:call("/webhooks/info.json", Params).

%% @doc
%% Update an existing webhook
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/webhooks.JSON.html#method=update)
%% @end
-spec update(map()) -> map().
update(Params) ->
  manderl:call("/webhooks/update.json", Params).

%% @doc
%% Delete an existing webhook
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/webhooks.JSON.html#method=delete)
%% @end
-spec delete(map()) -> map().
delete(Params) ->
  manderl:call("/webhooks/delete.json", Params).

