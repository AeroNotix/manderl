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
-module(manderl_templates).

-export([
         add/1,
         info/1,
         update/1,
         publish/1,
         delete/1,
         list/1,
         time_series/1,
         render/1
        ]).

%% @doc
%% Add a new template
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=add)
%% @end
-spec add(map()) -> map().
add(Params) ->
  manderl:call("/templates/add.json", Params).

%% @doc
%% Get the information for an existing template
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=info)
%% @end
-spec info(map()) -> map().
info(Params) ->
  manderl:call("/templates/info.json", Params).

%% @doc
%% Update the code for an existing template. If null is provided for any fields, the values will remain unchanged.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=update)
%% @end
-spec update(map()) -> map().
update(Params) ->
  manderl:call("/templates/update.json", Params).

%% @doc
%% Publish the content for the template. Any new messages sent using this template will start using the content that was previously in draft.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=publish)
%% @end
-spec publish(map()) -> map().
publish(Params) ->
  manderl:call("/templates/publish.json", Params).

%% @doc
%% Delete a template
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=delete)
%% @end
-spec delete(map()) -> map().
delete(Params) ->
  manderl:call("/templates/delete.json", Params).

%% @doc
%% Return a list of all the templates available to this user
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=list)
%% @end
-spec list(map()) -> map().
list(Params) ->
  manderl:call("/templates/list.json", Params).

%% @doc
%% Return the recent history (hourly stats for the last 30 days) for a template
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=time-series)
%% @end
-spec time_series(map()) -> map().
time_series(Params) ->
  manderl:call("/templates/time-series.json", Params).

%% @doc
%% Inject content and optionally merge fields into a template, returning the HTML that results
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/templates.JSON.html#method=render)
%% @end
-spec render(map()) -> map().
render(Params) ->
  manderl:call("/templates/render.json", Params).

