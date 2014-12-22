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
-module(manderl_subaccounts).

-export([
         list/1,
         add/1,
         info/1,
         update/1,
         delete/1,
         pause/1,
         resume/1
        ]).

%% @doc
%% Get the list of subaccounts defined for the account, optionally filtered by a prefix
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=list)
%% @end
-spec list(map()) -> map().
list(Params) ->
  manderl:call("/subaccounts/list.json", Params).

%% @doc
%% Add a new subaccount
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=add)
%% @end
-spec add(map()) -> map().
add(Params) ->
  manderl:call("/subaccounts/add.json", Params).

%% @doc
%% Given the ID of an existing subaccount, return the data about it
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=info)
%% @end
-spec info(map()) -> map().
info(Params) ->
  manderl:call("/subaccounts/info.json", Params).

%% @doc
%% Update an existing subaccount
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=update)
%% @end
-spec update(map()) -> map().
update(Params) ->
  manderl:call("/subaccounts/update.json", Params).

%% @doc
%% Delete an existing subaccount. Any email related to the subaccount will be saved, but stats will be removed and any future sending calls to this subaccount will fail.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=delete)
%% @end
-spec delete(map()) -> map().
delete(Params) ->
  manderl:call("/subaccounts/delete.json", Params).

%% @doc
%% Pause a subaccount's sending. Any future emails delivered to this subaccount will be queued for a maximum of 3 days until the subaccount is resumed.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=pause)
%% @end
-spec pause(map()) -> map().
pause(Params) ->
  manderl:call("/subaccounts/pause.json", Params).

%% @doc
%% Resume a paused subaccount's sending
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=resume)
%% @end
-spec resume(map()) -> map().
resume(Params) ->
  manderl:call("/subaccounts/resume.json", Params).

