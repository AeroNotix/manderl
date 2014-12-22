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
-module(manderl_messages).

-export([
         send/1,
         send_template/1,
         search/1,
         search_time_series/1,
         info/1,
         content/1,
         parse/1,
         send_raw/1,
         list_scheduled/1,
         cancel_scheduled/1,
         reschedule/1
        ]).

%% @doc
%% Send a new transactional message through Mandrill
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=send)
%% @end
-spec send(map()) -> map().
send(Params) ->
  manderl:call("/messages/send.json", Params).

%% @doc
%% Send a new transactional message through Mandrill using a template
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=send-template)
%% @end
-spec send_template(map()) -> map().
send_template(Params) ->
  manderl:call("/messages/send-template.json", Params).

%% @doc
%% Search recently sent messages and optionally narrow by date range, tags, senders, and API keys. If no date range is specified, results within the last 7 days are returned. This method may be called up to 20 times per minute. If you need the data more often, you can use /messages/info.json to get the information for a single message, or webhooks to push activity to your own application for querying.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=search)
%% @end
-spec search(map()) -> map().
search(Params) ->
  manderl:call("/messages/search.json", Params).

%% @doc
%% Search the content of recently sent messages and return the aggregated hourly stats for matching messages
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=search-time-series)
%% @end
-spec search_time_series(map()) -> map().
search_time_series(Params) ->
  manderl:call("/messages/search-time-series.json", Params).

%% @doc
%% Get the information for a single recently sent message
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=info)
%% @end
-spec info(map()) -> map().
info(Params) ->
  manderl:call("/messages/info.json", Params).

%% @doc
%% Get the full content of a recently sent message
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=content)
%% @end
-spec content(map()) -> map().
content(Params) ->
  manderl:call("/messages/content.json", Params).

%% @doc
%% Parse the full MIME document for an email message, returning the content of the message broken into its constituent pieces
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=parse)
%% @end
-spec parse(map()) -> map().
parse(Params) ->
  manderl:call("/messages/parse.json", Params).

%% @doc
%% Take a raw MIME document for a message, and send it exactly as if it were sent through Mandrill's SMTP servers
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=send-raw)
%% @end
-spec send_raw(map()) -> map().
send_raw(Params) ->
  manderl:call("/messages/send-raw.json", Params).

%% @doc
%% Queries your scheduled emails by sender or recipient, or both.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=list-scheduled)
%% @end
-spec list_scheduled(map()) -> map().
list_scheduled(Params) ->
  manderl:call("/messages/list-scheduled.json", Params).

%% @doc
%% Cancels a scheduled email.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=cancel-scheduled)
%% @end
-spec cancel_scheduled(map()) -> map().
cancel_scheduled(Params) ->
  manderl:call("/messages/cancel-scheduled.json", Params).

%% @doc
%% Cancels a scheduled email.
%%
%% see [Mandrill documentation](https://mandrillapp.com/api/docs/messages.JSON.html#method=reschedule)
%% @end
-spec reschedule(map()) -> map().
reschedule(Params) ->
  manderl:call("/messages/reschedule.json", Params).

