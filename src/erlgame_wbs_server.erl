%%%-------------------------------------------------------------------
%%% Created : 22 Dec 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc This file contains the websockets functions
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_wbs_server).

-author('Thiago Esteves').

%%%===================================================================
%%% Includes
%%%===================================================================

-include("erlgame.hrl").
%% For LOG purposes
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

%% cowboy callbacks
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

%%%===================================================================
%%% COWBOY local callbacks implementation
%%%===================================================================

websocket_init(State) ->
  ?LOG_INFO("Starting Websocket server at PID: ~p", [self()]),
  {[{text, <<"Erlgame is alive!">>}], State}.

websocket_handle({text, JsonBin}, State) ->
  ?LOG_INFO("~p", [JsonBin]),
  execute(jsone:decode(JsonBin)),
  {[], State}.

websocket_info(?SNAKE_SM_UPDATE_MSG(S,{Fx,Fy}), State) ->
  %% prepare Json file to be sent throught websockets
  #{ loop_map := NewMap} = lists:foldl(
    fun({X,Y}, #{counter := Acc, loop_map := M}) ->
      #{ snake := Snake} = M,
      #{ counter => Acc+1, 
         loop_map => M#{ snake => Snake#{
           erlgame_util:maybe_string_to_atom("p"++integer_to_list(Acc)) =>
           #{ x => X, y => Y}} }}
    end,
    #{counter => 0, loop_map => #{ food => #{x=> Fx, y => Fy},
                                   snake => #{} }},
    S),
  {[{text,jsone:encode(NewMap)}], State};

websocket_info(_Info, State) ->
  {[], State}.

%%%===================================================================
%%% API Implementation
%%%===================================================================
-spec init(map(), cowboy:req()) -> {any(), cowboy:req(), any()}.
init(Req0, Opts) ->
  logger:set_module_level(?MODULE, error),
  {cowboy_websocket, Req0, Opts}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Execute user operations based on the received message from
%%      webclient
%%
%% @param Map Map with client operation
%% @end
%%--------------------------------------------------------------------
-spec execute(map()) -> ok.
execute(#{<<"action">> := Action,<<"user">> := User}) ->
  erlgame_snake_sm:action(binary_to_list(User), binary_to_atom(Action));

execute(#{<<"user">> := User}) ->
  {ok, _} = erlgame_snake_sm:start_link({20,20}, 200),
  {ok, _} = erlgame_snake_sm:join_game(binary_to_list(User)),
  {ok, _} = erlgame_snake_sm:start_game(),
  ok.
