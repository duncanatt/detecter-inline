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
-module(util).
-author("Duncan Paul Attard").
-version("0.9").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").

%%% Public API exports.
-export([as_dir_name/1]).

%%% Internal exports.
-export([]).

%%% Type exports.
-export_type([]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Type declarations.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Appends the forward slash character to the specified directory path
%% if this does not exist.
%%
%% {@params
%%   {@name Dir}
%%   {@desc The directory path to append with `/'.}
%% }
%%
%% {@returns The dir name appended with `/'.}
-spec as_dir_name(Dir :: string()) -> Dir0 :: string().
as_dir_name([]) ->
  [$.];
as_dir_name([Char]) when Char =/= $/ ->
  [Char, $/];
as_dir_name(Last = [_]) ->
  Last;
as_dir_name([Char | Name]) ->
  [Char | as_dir_name(Name)].

%%% ----------------------------------------------------------------------------
%%% Internal exports.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Callbacks.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------
