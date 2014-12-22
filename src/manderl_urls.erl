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
-module(manderl_urls).

-export([
         list/0,
         search/1,
         time_series/1,
         tracking_domains/0,
         add_tracking_domain/1,
         check_tracking_domain/1
        ]).

%% @doc
%% Get the 100 most clicked URLs
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/urls.JSON.html#method=list)
%% @end
-spec list() -> map().
list() ->
  manderl:call("/urls/list.json", #{}).

%% @doc
%% Return the 100 most clicked URLs that match the search query given
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/urls.JSON.html#method=search)
%% @end
-spec search(map()) -> map().
search(Params) ->
  manderl:call("/urls/search.json", Params).

%% @doc
%% Return the recent history (hourly stats for the last 30 days) for a url
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/urls.JSON.html#method=time-series)
%% @end
-spec time_series(map()) -> map().
time_series(Params) ->
  manderl:call("/urls/time-series.json", Params).

%% @doc
%% Get the list of tracking domains set up for this account
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/urls.JSON.html#method=tracking-domains)
%% @end
-spec tracking_domains() -> map().
tracking_domains() ->
  manderl:call("/urls/tracking-domains.json", #{}).

%% @doc
%% Add a tracking domain to your account
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/urls.JSON.html#method=add-tracking-domain)
%% @end
-spec add_tracking_domain(map()) -> map().
add_tracking_domain(Params) ->
  manderl:call("/urls/add-tracking-domain.json", Params).

%% @doc
%% Checks the CNAME settings for a tracking domain. The domain must have been added already with the add-tracking-domain call
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/urls.JSON.html#method=check-tracking-domain)
%% @end
-spec check_tracking_domain(map()) -> map().
check_tracking_domain(Params) ->
  manderl:call("/urls/check-tracking-domain.json", Params).

