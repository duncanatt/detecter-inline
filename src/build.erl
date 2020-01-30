%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% @copyright 2019 Duncan Paul Attard
%%% @version 0.9
%%%
%%% @doc Builds EDoc.
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
-module(build).
-author("Duncan Paul Attard").
-version("0.9").

%%% Public API exports.
-export([edoc/0, leex/0, yecc/0]).

%%% Internal exports.
-export([]).

%%% Callback exports.
-export([]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% The directory of the compiled EDoc output.
-define(DOC_DIR, "doc").

%%% The name of the custom CSS file used by EDoc.
-define(CSS_FILE, "priv/edoc/edoc.css").

%%% The application name.
-define(APP_NAME, "detectEr Inline").

%%% EDoc macros to facilitate EDoc writing and visual parsing in source code.
-define(MACROS, [
  {returns, "<p><b>Returns:</b> {@?}</p>"},
  {params, "<dl>{@?}</dl>"},
  {name, "<dt>`{@?}'</dt>"},
  {desc, "<dd>{@?}</dd>"},
  {par, "<p>{@?}</p>"},
  {dl, "<dl>{@?}</dl>"},
  {ol, "<ol>{@?}</ol>"},
  {ul, "<ul>{@?}</ul>"},
  {item, "<li>{@?}</li>"},
  {term, "<dt>{@?}</dt>"},
  {eg, "<i>e.g.,</i>"},
  {ie, "<i>i.e.,</i>"},
  {etc, "<i>etc.</i>"},
  {ge, "&gt;="},
  {le, "&lt;="},
  {gt, "&gt;"},
  {lt, "&lt;"}
]).

%%% ----------------------------------------------------------------------------
%%% Type declarations.
%%% ----------------------------------------------------------------------------

%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

edoc() ->
  Params = [
    {title, ?APP_NAME},
    {dir, ?DOC_DIR},
    {stylesheet_file, ?CSS_FILE},
    {private, true},
    {def, ?MACROS},
    {todo, true}
%%    {doclet, edoc_doclet_mkdocs}
  ],
  edoc:application(detecter, ".", Params).


%%mkdoc() ->
%%  Params = [
%%    {title, ?APP_NAME},
%%%%    {dir, ?DOC_DIR},
%%%%    {stylesheet_file, ?CSS_FILE},
%%%%    {private, true},
%%    {def, ?MACROS},
%%%%    {todo, true},
%%    {doclet, edoc_doclet_mkdocs}
%%  ],
%%  edoc:application(detecter, ".", Params).


leex() ->
  leex:file("priv/hml_lexer.xrl", {scannerfile, "src/parsing/hml_lexer.erl"}).


yecc() ->
  yecc:file("priv/hml_parser.yrl", {parserfile, "src/parsing/hml_parser.erl"}).
