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
-module(idler).
-author("Duncan Paul Attard").
-version("0.9").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API exports.
-export([start/1]).
-export([mfa_spec/1]).

%%% Internal exports.
-export([loop/3]).

%%% Type exports.
-export_type([]).

-compile(export_all).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(KEY_EVENT_CNT, '$event_cnt').
-define(KEY_LAST_UPDATE, '$last_updated').
-define(KEY_TOTAL_IDLE, '$total_idle_time').

-define(REG_STATS_NAME, stats).

%% Keeps running statistics for running weighted mean, variance, etc.
-record(w_stats_ctx, {
  w_sum = 0 :: float(),
  w_sum_sq = 0 :: float(),
  mean = 0 :: float(),
  s = 0 :: float()
}).


%%% ----------------------------------------------------------------------------
%%% Type declarations.
%%% ----------------------------------------------------------------------------

-type w_stats_ctx() :: #w_stats_ctx{}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------
start(Total) ->
  register(?MODULE, Pid = spawn(?MODULE, loop, [1, Total, new_w_run_stats()])),
  Pid.

mfa_spec({server, loop, _}) ->
  {ok,
    fun({trace, _Server, spawned, _Launcher, {server, loop, [_]}}) ->
      ?TRACE("Processed trace 'spawned'."),
      put(?KEY_EVENT_CNT, 0),
      put(?KEY_TOTAL_IDLE, 0),
      put(?KEY_LAST_UPDATE, timestamp()),
      fun X() ->
        fun({trace, _Server, 'receive', {_, _, {add, A, B}}}) ->
          ?TRACE("Received 'add' event."),

          Idle = log_event(),
          ?TRACE("Time idle: ~b.", [Idle]),

          fun({trace, _Server, send, {_, {add, AB}}, _}) ->

            % Unfold monitor to process next trace event message.
            X();
            (_) ->
              'end'
          end;
          ({trace, _Server, 'receive', {_, _, {mul, A, B}}}) ->
            ?TRACE("Received 'mul' event."),

            Idle = log_event(),
            ?TRACE("Time idle: ~b.", [Idle]),

            fun({trace, _Server, send, {_, {mul, AB}}, _}) ->

              % Unfold monitor to process next trace event message.
              X();
              (_) ->
                'end'
            end;
          ({trace, _Server, 'receive', {_, _, stop}}) ->
            ?TRACE("Received 'stop' event, stats:"),

            log_event(),

            Now = timestamp(),
            Total = Now - get(?KEY_LAST_UPDATE),

            Mean = case get(?KEY_EVENT_CNT) of 0 -> Total; N -> Total / N end,


            ?TRACE("Total events: ~b.", [get(?KEY_EVENT_CNT)]),
            ?TRACE("Total idle time: ~bms", [Total]),
            ?TRACE("Mean idle time: ~.2fms", [Mean]),


            case catch ?MODULE !
              {stats, self(), get(?KEY_EVENT_CNT), get(?KEY_TOTAL_IDLE)} of
              {'EXIT', _} ->
                ?WARN("Profiler not alive!");
              _ ->
                ok
            end,


            % Return the result to main collector monitor.
            'end';
          (_) ->
            'end'
        end
      end();
      (_) ->
        'end'
    end};
mfa_spec(_) ->
  undefined.


%%% ----------------------------------------------------------------------------
%%% Internal exports.
%%% ----------------------------------------------------------------------------

loop(Count, Total, WStatsCtx = #w_stats_ctx{}) ->
  receive
    {stats, _From, EventCnt, TotalIdle} ->
      WStatsCtx0 = w_run_stats(WStatsCtx, EventCnt, TotalIdle),

      ?TRACE("Summing loop received stats."),
      if Count < Total ->
        loop(Count + 1, Total, WStatsCtx0);
        true ->
          io:format("Exited loop with mean idle time ~.2fms.~n", [
            w_run_mean(WStatsCtx0)
          ])
      end;
    Any ->
      io:format("WARN: Receive unknown message ~p.~n", [Any])
  end.

-spec new_w_run_stats() -> w_stats_ctx().
new_w_run_stats() ->
  #w_stats_ctx{w_sum = 0.0, w_sum_sq = 0.0, mean = 0.0, s = 0.0}.

-spec w_run_stats(Ctx, Weight, Sample) -> w_stats_ctx()
  when
  Ctx :: w_stats_ctx(),
  Weight :: float(),
  Sample :: float().
w_run_stats(#w_stats_ctx{w_sum = WSum, w_sum_sq = WSumSq, mean = Mean, s = S},
    Weight, Sample) ->
  WSum0 = WSum + Weight,
  WSumSq0 = WSumSq + Weight * Weight,
  Mean0 = Mean + (Weight / WSum0) * (Sample - Mean),
  S0 = S + Weight * (Sample - Mean) * (Sample - Mean0),
  #w_stats_ctx{w_sum = WSum0, w_sum_sq = WSumSq0, mean = Mean0, s = S0}.

-spec w_run_mean(Ctx :: w_stats_ctx()) -> float().
w_run_mean(#w_stats_ctx{mean = Mean}) ->
  Mean.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

%% @private Returns the current operating system time in milliseconds.
-spec timestamp() -> pos_integer().
timestamp() ->
  os:system_time(millisecond).

%% @private Logs the event and calculations in the process dictionary and
%% returns the last idle time.
-spec log_event() -> float().
log_event() ->
  Now = timestamp(),
  Idle = Now - get(?KEY_LAST_UPDATE),
  put(?KEY_TOTAL_IDLE, get(?KEY_TOTAL_IDLE) + Idle),
  put(?KEY_LAST_UPDATE, Now),
  put(?KEY_EVENT_CNT, get(?KEY_EVENT_CNT) + 1),
  Idle.



