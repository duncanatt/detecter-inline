%%%-------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% @copyright (C) 2019 Duncan Paul Attard
%%% @version 0.9
%%%
%%% @doc
%%%
%%% @end
%%% Created: 14. Oct 2019
%%% 
%%% Copyright (c) 2019 Duncan Paul Attard <duncanatt@gmail.com>
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
-module(monitor).
-author("Duncan Paul Attard").
-version("0.9").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API exports.
-export([dispatch/1, filter/1]).

%%% Type exports.
-export_type([monitor/0, mfa_spec/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(MONITOR, '$monitor').
-define(VERDICT_YES, yes).
-define(VERDICT_NO, no).
-define(VERDICT_END, 'end').

%%% ----------------------------------------------------------------------------
%%% Type declarations.
%%% ----------------------------------------------------------------------------

-type verdict() :: ?VERDICT_YES | ?VERDICT_NO | ?VERDICT_END.
%% Three monitor definitive verdicts.

-type monitor() :: fun((Event :: term()) -> verdict() | no_return()).
%% Monitor that accepts an event and transitions to the next monitor unfolding
%% or a definitive verdict when the monitor cannot transition further. Monitors
%% can also diverge indefinitely, in which case events are consumed albeit the
%% final definitive verdict is never reached.

-type mfa_spec() :: fun((Mfa :: mfa()) -> {ok, monitor()} | undefined).
%% Function that specifies which remote (<i>i.e.,</i> external) function calls
%% are to be monitored.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Dispatches the specified abstract event to the monitor for analysis.
%%
%% {@params
%%   {@name Event}
%%   {@desc The abstract event that the monitor is to analyze.}
%% }
%%
%% {@returns Depends on the event type. See {@link trace_lib:event/0}.
%%           {@ul
%%             {@item When event is of type `fork', the PID of the new child
%%                    process is returned;
%%             }
%%             {@item When event is of type `init', the PID of the parent
%%                    process is returned;
%%             }
%%             {@item When event is of type `exit', the exit reason is returned;}
%%             {@item When event is of type `send', the message is returned;}
%%             {@item When event is of type `recv', the message is returned.}
%%           }
%% }
-spec dispatch(Event :: events:event()) -> term().
dispatch(Event = {fork, _Parent, Child, _Mfa}) ->
  do_monitor(events:to_evm_event(Event),
    fun(Verdict) -> ?INFO("Reached verdict '~s' after ~w.", [Verdict, Event]) end
  ),
  Child;
dispatch(Event = {init, _Child, Parent, _Mfa}) ->
  do_monitor(events:to_evm_event(Event),
    fun(Verdict) -> ?INFO("Reached verdict '~s' after ~w.", [Verdict, Event]) end
  ),
  Parent;
dispatch(Event = {exit, _Process, Reason}) ->
  do_monitor(events:to_evm_event(Event),
    fun(Verdict) -> ?INFO("Reached verdict '~s' after ~w.", [Verdict, Event]) end
  ),
  Reason;
dispatch(Event = {send, _Sender, _Receiver, Msg}) ->
  do_monitor(events:to_evm_event(Event),
    fun(Verdict) -> ?INFO("Reached verdict '~s' after ~w.", [Verdict, Event]) end
  ),
  Msg;
dispatch(Event = {recv, _Receiver, Msg}) ->
  do_monitor(events:to_evm_event(Event),
    fun(Verdict) -> ?INFO("Reached verdict '~s' after ~w.", [Verdict, Event]) end
  ),
  Msg.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @private Determines whether the specified monitor is indeed a verdict.
-spec is_verdict(Verdict :: term()) -> boolean().
is_verdict(Verdict) when Verdict =:= yes; Verdict =:= no; Verdict =:= 'end' ->
  true;
is_verdict(_) ->
  false.

%% @private Retrieves the monitor function stored in the process dictionary (if
%% any), and applies it on the event. The result is put back in the process
%% dictionary. If a verdict state is reached, the callback function is invoked,
%% otherwise nothing is done. When no monitor function is stored inside the
%% process dictionary (i.e. meaning that the process is not monitored), the atom
%% `undefined' is returned.
-spec do_monitor(Event, VerdictFun) -> Monitor :: monitor() | undefined
  when
  Event :: trace_lib:evm_event(),
  VerdictFun :: fun((Verdict :: verdict()) -> any()).
do_monitor(Event, VerdictFun) when is_function(VerdictFun, 1) ->
  case get(?MONITOR) of
    undefined ->
      ?TRACE("Monitor not defined; not handling trace event ~w.", [Event]),
      undefined;
    Monitor ->

      % Analyze event. At this point, monitor might have reached a verdict.
      % Check whether verdict is reached to enable immediate detection, should
      % this be the case.
      put(?MONITOR, Monitor0 = analyze(Monitor, Event)),
      case is_verdict(Monitor0) of
        true ->
          VerdictFun(Monitor0);
        false ->
          ok
      end,
      Monitor0
  end.

%% @private Effects the analysis by applying the monitor function to the
%% specified event. If a verdict state is reached, the event is silently
%% discarded.
-spec analyze(Monitor, Event) -> Monitor0 :: monitor()
  when
  Monitor :: monitor(),
  Event :: trace_lib:event().
analyze(Monitor, Event) ->
  case is_verdict(Monitor) of
    true ->

      % Monitor is at the verdict state, and the event, even though it is
      % analyzed, does not alter the current verdict.
      ?TRACE("Monitor analyzing event ~w and reached verdict '~s'.",
        [Event, Monitor]),
      Monitor;
    false ->

      % Monitor is not yet at the verdict state and can analyze the event.
      % Return next monitor unfolding.
      ?TRACE("Monitor analyzing event ~w.", [Event]),
      Monitor(Event)
  end.

%% @doc Default filter that allows all events to pass.
-spec filter(Event :: events:event()) -> true.
filter(_) ->
  true. % True = keep event.