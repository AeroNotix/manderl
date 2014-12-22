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
-module(manderl_metadata).

-export([
         list/0,
         add/1,
         update/1,
         delete/1
        ]).

%% @doc
%% Get the list of custom metadata fields indexed for the account.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/metadata.JSON.html#method=list)
%% @end
-spec list() -> map().
list() ->
  manderl:call("/metadata/list.json", #{}).

%% @doc
%% Add a new custom metadata field to be indexed for the account.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/metadata.JSON.html#method=add)
%% @end
-spec add(map()) -> map().
add(Params) ->
  manderl:call("/metadata/add.json", Params).

%% @doc
%% Update an existing custom metadata field.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/metadata.JSON.html#method=update)
%% @end
-spec update(map()) -> map().
update(Params) ->
  manderl:call("/metadata/update.json", Params).

%% @doc
%% Delete an existing custom metadata field. Deletion isn't instataneous, and /metadata/list will continue to return the field until the asynchronous deletion process is complete.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/metadata.JSON.html#method=delete)
%% @end
-spec delete(map()) -> map().
delete(Params) ->
  manderl:call("/metadata/delete.json", Params).

