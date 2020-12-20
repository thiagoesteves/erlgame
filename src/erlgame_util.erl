%%%-------------------------------------------------------------------
%%% Created : 18 Dec 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc This file generic APIs to use in the project
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_util).

-author('Thiago Esteves').

%%%===================================================================
%%% Includes
%%%===================================================================

-include("erlgame.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

%% Public API
-export([maybe_string_to_atom/1,
         print_game/5]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Try to create a atom from a list or use an existing one
%%
%% @param Str String to be converted
%% @end
%%--------------------------------------------------------------------
-spec maybe_string_to_atom(Str :: list()) -> atom().
maybe_string_to_atom(Str) when is_list(Str) ->
  try 
    erlang:list_to_existing_atom(Str)
  catch
    _:_ -> erlang:list_to_atom(Str)
  end.

%%--------------------------------------------------------------------
%% @doc Print the current position in ASCII
%%
%% @param MaxX Maximum X position
%% @param MaxY Maximum Y position
%% @param Px Current X position
%% @param Py Current Y position
%% @param Potion Potion position
%% @end
%%--------------------------------------------------------------------
print_game(MaxX,MaxY,Px,Py,Potion) ->
  io:format("\ec~n"),
  lists:foreach(
    fun(Y) ->
      io:format("|"),
      lists:foreach(
          fun(X) ->
            case {X,Y} of
              {Px,Py} -> io:format("+");
              Potion -> io:format("O");
              _-> io:format(" ")
              end
          end,
          lists:seq(0,MaxX)),
      io:format("|~n")
    end,
    lists:reverse(lists:seq(0,MaxY)) ).

%%====================================================================
%% Internal functions
%%====================================================================
