% @hidden
-module(manderl).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, call/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

call(Path, Data) ->
  try
    gen_server:call(?SERVER, {call, Path, Data})
  catch
    _:_ -> {error, timeout}
  end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, #{
     endpoint => eutils:to_binary(application:get_env(manderl, endpoint, "https://mandrillapp.com/api/1.0/")),
     api_key => eutils:to_binary(application:get_env(manderl, api_key, undefined))
    }}.

handle_call({call, Path, Data}, _From, #{api_key := ApiKey, endpoint := Endpoint} = State) ->
  ReqBody = jsx:encode(maps:merge(Data, #{key => ApiKey})),
  ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
  ReqURL = <<Endpoint/binary, (eutils:to_binary(Path))/binary>>,
  Method = post,
  case hackney:request(Method, ReqURL, ReqHeaders, stream, []) of
    {ok, ClientRef} ->
      case hackney:send_body(ClientRef, ReqBody) of
        ok ->
          case hackney:start_response(ClientRef) of
            {ok, Status, _Headers, ClientRef} ->
              case hackney:body(ClientRef) of
                {ok, Body} ->
                  Res = if 
                          Status == 200 -> ok;
                          true -> error
                        end,
                  {reply, {Res, Status, jsx:decode(Body, [{labels, atom}, return_maps])}, State};
                {error, E} ->
                  {reply, {error, E}, State}
              end;
            _ ->
              {reply, {error, invalid_response}, State}
          end;
        _ ->
          {reply, {error, invalid_request_body}, State}
      end;
    _ ->
      {reply, {error, request_failed}, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

