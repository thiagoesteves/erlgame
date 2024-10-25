%%%-------------------------------------------------------------------
%%% Created : 14 Dec 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc This file contains the app start/stop point
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_app).

-behaviour(application).

%%====================================================================
%% API functions
%%====================================================================

-export([start/2, stop/1]).

%%====================================================================
%% API functions implementation
%%====================================================================

start(_StartType, _StartArgs) ->
%% Retrieve the port from the environment variable
PortString = os:getenv("PORT"),
Port = case list_to_integer(PortString) of
    {error, _} -> 4000; %% Fallback to default port
    Value -> Value
end,
Dispatch = cowboy_router:compile([
    {'_', [
            {"/",             cowboy_static,      {priv_file, erlgame, "index.html"}},
            {"/websocket",    erlgame_wbs_server, []},
            {"/static/[...]", cowboy_static,      {priv_dir, erlgame, "static"}}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
    env => #{dispatch => Dispatch}
  }),
  erlgame_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
