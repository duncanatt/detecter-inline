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
-module(client).
-author("Duncan Paul Attard").
-version("0.9").

%%% Public API exports.
-export([add/2, mul/2]).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Issues addition request to server.
add(A, B) ->
 {add, AB} = util:rpc(server, {add, A, B}),
  AB.

%% @doc Issues multiplication request to server.
mul(A, B) ->
  {mul, AB} = util:rpc(server, {mul, A, B}),
  AB.

