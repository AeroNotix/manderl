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
-module(manderl_inbound).

-export([
         domains/0,
         add_domain/1,
         check_domain/1,
         delete_domain/1,
         routes/1,
         add_route/1,
         update_route/1,
         delete_route/1,
         send_raw/1
        ]).

%% @doc
%% List the domains that have been configured for inbound delivery
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=domains)
%% @end
-spec domains() -> map().
domains() ->
  manderl:call("/inbound/domains.json", #{}).

%% @doc
%% Add an inbound domain to your account
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=add-domain)
%% @end
-spec add_domain(map()) -> map().
add_domain(Params) ->
  manderl:call("/inbound/add-domain.json", Params).

%% @doc
%% Check the MX settings for an inbound domain. The domain must have already been added with the add-domain call
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=check-domain)
%% @end
-spec check_domain(map()) -> map().
check_domain(Params) ->
  manderl:call("/inbound/check-domain.json", Params).

%% @doc
%% Delete an inbound domain from the account. All mail will stop routing for this domain immediately.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=delete-domain)
%% @end
-spec delete_domain(map()) -> map().
delete_domain(Params) ->
  manderl:call("/inbound/delete-domain.json", Params).

%% @doc
%% List the mailbox routes defined for an inbound domain
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=routes)
%% @end
-spec routes(map()) -> map().
routes(Params) ->
  manderl:call("/inbound/routes.json", Params).

%% @doc
%% Add a new mailbox route to an inbound domain
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=add-route)
%% @end
-spec add_route(map()) -> map().
add_route(Params) ->
  manderl:call("/inbound/add-route.json", Params).

%% @doc
%% Update the pattern or webhook of an existing inbound mailbox route. If null is provided for any fields, the values will remain unchanged.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=update-route)
%% @end
-spec update_route(map()) -> map().
update_route(Params) ->
  manderl:call("/inbound/update-route.json", Params).

%% @doc
%% Delete an existing inbound mailbox route
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=delete-route)
%% @end
-spec delete_route(map()) -> map().
delete_route(Params) ->
  manderl:call("/inbound/delete-route.json", Params).

%% @doc
%% Take a raw MIME document destined for a domain with inbound domains set up, and send it to the inbound hook exactly as if it had been sent over SMTP
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=send-raw)
%% @end
-spec send_raw(map()) -> map().
send_raw(Params) ->
  manderl:call("/inbound/send-raw.json", Params).

