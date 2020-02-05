%%%-------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% @copyright (C) 2020 Duncan Paul Attard
%%% @version 0.9
%%%
%%% @doc
%%%
%%% @end
%%% Created: 05. Feb 2020
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
-module(events).
-author("Duncan Paul Attard").
-version("0.9").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").

%%% Public API exports.
-export([to_evm_event/1]).

%%% Internal exports.
-export([]).

%%% Type exports.
-export_type([event/0, evm_event/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Type declarations.
%%% ----------------------------------------------------------------------------

-type event() ::
{fork, Parent :: pid(), Child :: pid(), Mfa :: mfa()} |
{init, Child :: pid(), Parent :: pid(), Mfa :: mfa()} |
{exit, Process :: pid(), Reason :: term()} |
{send, Sender :: pid(), Receiver :: pid(), Message :: term()} |
{recv, Receiver :: pid(), Message :: term()}.
%% Abstract trace event that is not linked to a specific tracer implementation.
%% See {@link evm_event/0} for trace events specific to the EVM.

-type evm_event() ::
{trace, PidSrc :: pid(), spawn, PidTgt :: pid(), Mfa :: mfa()} |
{trace, PidSrc :: pid(), spawned, PidTgt :: pid(), Mfa :: mfa()} |
{trace, PidSrc :: pid(), exit, Reason :: term()} |
{trace, PidSrc :: pid(), send, Msg :: term(), PidTgt :: pid()} |
{trace, PidSrc :: pid(), 'receive', Msg :: term()}.
%% Trace event encoded as a trace message that is issued by the EVM built-in
%% tracing mechanism (see {@link erlang:trace/3} for more information).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Translates the log event to one that is identical to what the EVM
%% generates.
%% {@params
%%   {@name Event}
%%   {@desc The event to translate to EVM format.}
%% }
%%
%% {@returns The translated event.}
-spec to_evm_event(Event :: event()) -> Event0 :: evm_event().
to_evm_event({fork, Parent, Child, Mfa}) ->
  {trace, Parent, spawn, Child, Mfa};
to_evm_event({init, Child, Parent, Mfa}) ->
  {trace, Child, spawned, Parent, Mfa};
to_evm_event({exit, Process, Reason}) ->
  {trace, Process, exit, Reason};
to_evm_event({send, Sender, Receiver, Msg}) ->
  {trace, Sender, send, Msg, Receiver};
to_evm_event({recv, Receiver, Msg}) ->
  {trace, Receiver, 'receive', Msg}.
