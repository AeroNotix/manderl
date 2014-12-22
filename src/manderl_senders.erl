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
-module(manderl_senders).

-export([
         list/0,
         domains/0,
         add_domain/1,
         check_domain/1,
         verify_domain/1,
         info/1,
         time_series/1
        ]).

%% @doc
%% Return the senders that have tried to use this account.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=list)
%% @end
-spec list() -> map().
list() ->
  manderl:call("/senders/list.json", #{}).

%% @doc
%% Returns the sender domains that have been added to this account.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=domains)
%% @end
-spec domains() -> map().
domains() ->
  manderl:call("/senders/domains.json", #{}).

%% @doc
%% 
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=add-domain)
%% @end
-spec add_domain(map()) -> map().
add_domain(Params) ->
  manderl:call("/senders/add-domain.json", Params).

%% @doc
%% Checks the SPF and DKIM settings for a domain. If you haven't already added this domain to your account, it will be added automatically.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=check-domain)
%% @end
-spec check_domain(map()) -> map().
check_domain(Params) ->
  manderl:call("/senders/check-domain.json", Params).

%% @doc
%% Sends a verification email in order to verify ownership of a domain. Domain verification is an optional step to confirm ownership of a domain. Once a domain has been verified in a Mandrill account, other accounts may not have their messages signed by that domain unless they also verify the domain. This prevents other Mandrill accounts from sending mail signed by your domain.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=verify-domain)
%% @end
-spec verify_domain(map()) -> map().
verify_domain(Params) ->
  manderl:call("/senders/verify-domain.json", Params).

%% @doc
%% Return more detailed information about a single sender, including aggregates of recent stats
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=info)
%% @end
-spec info(map()) -> map().
info(Params) ->
  manderl:call("/senders/info.json", Params).

%% @doc
%% Return the recent history (hourly stats for the last 30 days) for a sender
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=time-series)
%% @end
-spec time_series(map()) -> map().
time_series(Params) ->
  manderl:call("/senders/time-series.json", Params).

