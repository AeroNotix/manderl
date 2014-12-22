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
-module(manderl_ips).

-export([
         list/0,
         info/1,
         provision/1,
         start_warmup/1,
         cancel_warmup/1,
         set_pool/1,
         delete/1,
         list_pools/0,
         pool_info/1,
         create_pool/1,
         delete_pool/1,
         check_custom_dns/1,
         set_custom_dns/1
        ]).

%% @doc
%% Lists your dedicated IPs.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=list)
%% @end
-spec list() -> map().
list() ->
  manderl:call("/ips/list.json", #{}).

%% @doc
%% Retrieves information about a single dedicated ip.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=info)
%% @end
-spec info(map()) -> map().
info(Params) ->
  manderl:call("/ips/info.json", Params).

%% @doc
%% Requests an additional dedicated IP for your account. Accounts may have one outstanding request at any time, and provisioning requests are processed within 24 hours.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=provision)
%% @end
-spec provision(map()) -> map().
provision(Params) ->
  manderl:call("/ips/provision.json", Params).

%% @doc
%% Begins the warmup process for a dedicated IP. During the warmup process, Mandrill will gradually increase the percentage of your mail that is sent over the warming-up IP, over a period of roughly 30 days. The rest of your mail will be sent over shared IPs or other dedicated IPs in the same pool.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=start-warmup)
%% @end
-spec start_warmup(map()) -> map().
start_warmup(Params) ->
  manderl:call("/ips/start-warmup.json", Params).

%% @doc
%% Cancels the warmup process for a dedicated IP.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=cancel-warmup)
%% @end
-spec cancel_warmup(map()) -> map().
cancel_warmup(Params) ->
  manderl:call("/ips/cancel-warmup.json", Params).

%% @doc
%% Moves a dedicated IP to a different pool.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=set-pool)
%% @end
-spec set_pool(map()) -> map().
set_pool(Params) ->
  manderl:call("/ips/set-pool.json", Params).

%% @doc
%% Deletes a dedicated IP. This is permanent and cannot be undone.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=delete)
%% @end
-spec delete(map()) -> map().
delete(Params) ->
  manderl:call("/ips/delete.json", Params).

%% @doc
%% Lists your dedicated IP pools.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=list-pools)
%% @end
-spec list_pools() -> map().
list_pools() ->
  manderl:call("/ips/list-pools.json", #{}).

%% @doc
%% Describes a single dedicated IP pool.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=pool-info)
%% @end
-spec pool_info(map()) -> map().
pool_info(Params) ->
  manderl:call("/ips/pool-info.json", Params).

%% @doc
%% Creates a pool and returns it. If a pool already exists with this name, no action will be performed.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=create-pool)
%% @end
-spec create_pool(map()) -> map().
create_pool(Params) ->
  manderl:call("/ips/create-pool.json", Params).

%% @doc
%% Deletes a pool. A pool must be empty before you can delete it, and you cannot delete your default pool.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=delete-pool)
%% @end
-spec delete_pool(map()) -> map().
delete_pool(Params) ->
  manderl:call("/ips/delete-pool.json", Params).

%% @doc
%% Tests whether a domain name is valid for use as the custom reverse DNS for a dedicated IP.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=check-custom-dns)
%% @end
-spec check_custom_dns(map()) -> map().
check_custom_dns(Params) ->
  manderl:call("/ips/check-custom-dns.json", Params).

%% @doc
%% Configures the custom DNS name for a dedicated IP.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/ips.JSON.html#method=set-custom-dns)
%% @end
-spec set_custom_dns(map()) -> map().
set_custom_dns(Params) ->
  manderl:call("/ips/set-custom-dns.json", Params).

