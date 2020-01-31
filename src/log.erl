%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% @copyright 2019 Duncan Paul Attard
%%% @version 0.9
%%%
%%% @doc Implementation of a logging API providing the backend to the logging
%%% macros exposed in `log.hrl'.
%%%
%%% @end
%%% Created: 15. Jul 2019 16:18
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
%%% ----------------------------------------------------------------------------
-module(log).
-include("log.hrl").

%%% API exports.
-export([config_file/0, write/4, write/5]).


%%% ------------------------------------------------------------------------ %%%
%%% Client API.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

config_file() ->
  {ok, Log} = file:open("log", [write]),
  erlang:group_leader(Log, self()).

%% -----------------------------------------------------------------------------
%% Writes the log string to the output.
%% write(LogLevelStr, Module, Line, Format) where:
%%   * LogLevelStr::string() is the log string output next to each log ("TRACE",
%%     "DEBUG, "INFO", "ERROR"), see log.hrl.
%%   * Module::atom() is the module name performing the log.
%%   * Line::integer() is the line number where the log is performed.
%%   * Format::string() is the standard format string used by io:format/1-3
%%     functions.
%% Returns: the no-op ok.
%% -----------------------------------------------------------------------------
write(LogLevelStr, Module, Line, Format) ->
  write(LogLevelStr, Module, Line, Format, []).

%% -----------------------------------------------------------------------------
%% Writes the log string to the output.
%% write(LogLevelStr, Module, Line, Format) where:
%%   * LogLevelStr::string() is the log string output next to each log ("TRACE",
%%     "DEBUG, "INFO", "ERROR"), see log.hrl.
%%   * Module::atom() is the module name performing the log.
%%   * Line::integer() is the line number where the log is performed.
%%   * Format::string() is the standard format string used by io:format/1-3
%%     functions.
%%   * Params::list() is the list of parameters that are used by the Format
%%     string.
%% Returns: the no-op ok.
%% -----------------------------------------------------------------------------
write(LogLevelStr, Module, Line, Format, Params) ->
  case can_log(?log_level, LogLevelStr) of
    true ->
      io:fwrite(user, "[~s - ~p - ~p:~p] - ~s~n",
        [LogLevelStr, self(), Module, Line, io_lib:format(Format, Params)]);
%%      io:fwrite(standard_io, "[~s - ~p - ~p:~p] - ~s~n",
%%        [LogLevelStr, self(), Module, Line, io_lib:format(Format, Params)]);
    false -> ok
  end.


%%% ------------------------------------------------------------------------ %%%
%%% Helper functions.                                                        %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Determines whether a log message can be output, given the specified trace
%% log level and log level string (see log.hrl).
%% can_log(LogLevel, LogLevelStr) where:
%%   * LogLevel::integer() is the trace level from 1 to 5 (1 = TRACE, 2 = DEBUG,
%%     3 = INFO, 4 = WARN, 5 = ERROR).
%%   * LogLevelStr::string() is the log string output next to each log ("TRACE",
%%     "DEBUG, "INFO", "WARN", "ERROR").
%% Returns: true if the log statement can be output.
%%          | false if the log statement cannot be output.
%% -----------------------------------------------------------------------------
can_log(?trace_level, ?trace_str) ->
  true;
can_log(?trace_level, ?debug_str) ->
  true;
can_log(?trace_level, ?info_str) ->
  true;
can_log(?trace_level, ?warn_str) ->
  true;
can_log(?trace_level, ?error_str) ->
  true;

can_log(?debug_level, ?debug_str) ->
  true;
can_log(?debug_level, ?info_str) ->
  true;
can_log(?debug_level, ?warn_str) ->
  true;
can_log(?debug_level, ?error_str) ->
  true;

can_log(?info_level, ?info_str) ->
  true;
can_log(?info_level, ?warn_str) ->
  true;
can_log(?info_level, ?error_str) ->
  true;

can_log(?warn_level, ?warn_str) ->
  true;
can_log(?warn_level, ?error_str) ->
  true;

can_log(?error_level, ?error_str) ->
  true;
can_log(_, _) -> false.
