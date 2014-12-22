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
-module(manderl_users).

-export([
         info/0,
         ping/0,
         senders/0
        ]).

%% @doc
%% Return the information about the API-connected user
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/users.JSON.html#method=info)
%% @end
-spec info() -> map().
info() ->
  manderl:call("/users/info.json", #{}).

%% @doc
%% Validate an API key and respond to a ping
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/users.JSON.html#method=ping2)
%% @end
-spec ping() -> map().
ping() ->
  manderl:call("/users/ping2.json", #{}).

%% @doc
%% Return the senders that have tried to use this account, both verified and unverified
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/users.JSON.html#method=senders)
%% @end
-spec senders() -> map().
senders() ->
  manderl:call("/users/senders.json", #{}).
