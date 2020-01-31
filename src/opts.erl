%%%-------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% @copyright (C) 2020 Duncan Paul Attard
%%% @version 0.9
%%%
%%% @doc
%%%
%%% @end
%%% Created: 31. Jan 2020
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
-module(opts).
-author("Duncan Paul Attard").
-version("0.9").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").

%%% Public API exports.
-export([file_ext/1, compile_opts/1]).
-export([out_dir_opt/1, include_opt/1, erl_opt/1, verbose_opt/1]).

%%% Internal exports.
-export([]).

%%% Type exports.
-export_type([option/0, options/0]).
-export_type([directory/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% File extensions.
-define(EXT_ERL, ".erl").
-define(EXT_BEAM, ".beam").

-define(MFA_SPEC, mfa_spec).

%% Option definitions and their values.
-define(OPT_INCLUDE, i). % Kept same option name as Erlang compiler.
-define(OPT_OUT_DIR, outdir). % Kept same option name as Erlang compiler.
-define(OPT_ERL, erl).
-define(OPT_VERBOSE, v).
-define(OPT_FILTER, filter).

%% Default Erlang compiler options.
-define(COMPILER_OPTS, [nowarn_shadow_vars, return]).


%%% ----------------------------------------------------------------------------
%%% Type declarations.
%%% ----------------------------------------------------------------------------

-type option() :: {outdir, directory()} | {i, directory()} | erl | v.
%% Compiler options.

-type options() :: [option()].
%% Compiler option list.

-type directory() :: string().
%% Directory.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Returns the `.erl' file extension if `erl' option is set, otherwise
%% `.beam'.
file_ext(Opts) ->
  case erl_opt(Opts) of true -> ?EXT_ERL; false -> ?EXT_BEAM end.

%% @doc Configures the Erlang compiler options used to generate the beam
%% code file.
-spec compile_opts(Opts :: options()) -> CompileOpts :: [compile:option()].
compile_opts(Opts) ->
  [{i, out_dir_opt(Opts)} | ?COMPILER_OPTS].

%% @doc Returns the compiler output directory option if defined. Defaults to
%% '.'.
-spec out_dir_opt(Opts :: options()) -> Dir :: string().
out_dir_opt(Opts) ->
  proplists:get_value(?OPT_OUT_DIR, Opts, ".").

%% @doc Returns the compiler include directory option if defined. Defaults
%% to '.'.
-spec include_opt(Opts :: options()) -> Dir :: string().
include_opt(Opts) ->
  proplists:get_value(?OPT_INCLUDE, Opts, ".").

%% @doc Returns the compiler switch that determines whether Erlang source
%% code is output instead of beam. Defaults to 'false'.
-spec erl_opt(Opts :: options()) -> Flag :: boolean().
erl_opt(Opts) ->
  proplists:get_value(?OPT_ERL, Opts, false).

%% @doc Returns the compiler switch that determines whether the resulting
%% monitor includes logging statements for debugging purposes. Defaults to
%% 'false'.
-spec verbose_opt(Opts :: options()) -> Flag :: boolean().
verbose_opt(Opts) ->
  proplists:get_value(?OPT_VERBOSE, Opts, false).
