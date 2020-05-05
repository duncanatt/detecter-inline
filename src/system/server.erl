%%%-------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% @copyright (C) 2020 Duncan Paul Attard
%%% @version 0.9
%%%
%%% @doc
%%%
%%% @end
%%% Created: 30. Jan 2020
%%% 
%%% Copyright (c) 2020 Duncan Paul Attard <duncanatt@gmail.com>
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------
-module(server).
-author("Duncan Paul Attard").
-version("0.9").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").

%%% Public API exports.
-export([start/1, stop/0]).

%%% Internal exports.
-export([loop/1]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Normal and buggy operation constants.
-define(MODE_OK, ok).
-define(MODE_BUGGY, buggy).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Starts server.
start(Mode) ->
  register(?MODULE, Pid = spawn(?MODULE, loop, [
    if Mode =:= ?MODE_OK -> 0; Mode =:= ?MODE_BUGGY -> 1 end
  ])),
  Pid.

%% @doc Stops server.
stop() ->
  util:rpc(?MODULE, stop).


%%% ----------------------------------------------------------------------------
%%% Internal exports.
%%% ----------------------------------------------------------------------------

%% @private Main server loop.
loop(ErrFact) ->
  receive
    {From, Ref, {add, A, B}} ->

      % Handle addition request from client.
      From ! {Ref, {add, A + B + ErrFact}},
      loop(ErrFact);

    {From, Ref, {mul, A, B}} ->

      % Handle multiplication request from client.
      From ! {Ref, {mul, A * B + ErrFact}},
      loop(ErrFact);

    {From, Ref, stop} ->

      % Handle stop request. Server does not loop again.
      From ! {Ref, {ok, stopped}};

    Any ->
      io:format("WARN: unknown request ~w.~n", [Any]),
      loop(ErrFact)
  end.
