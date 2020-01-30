%%% ----------------------------------------------------------------------------
%%% Regular expression classes.
%%% ----------------------------------------------------------------------------

% on {M, F, A} when [Guards] attach [Phi].

Definitions.

% Digit characters.
DIGIT                           = [0-9]

% Lowercase characters.
LOWER                           = [a-z]

% Uppercase characters.
UPPER                           = [A-Z]

% Uppercase characters with underscore.
UPPER_                          = [A-Z_]

% Alpha-numeric token that includes the underscore symbol.
ALPHA                           = [a-zA-Z0-9_]

% Boolean operators.
BOOL_OP                         = (and|or|andalso|orelse|not|xor)

% Bitwise operators.
BIT_OP                          = (bnot|band|bor|bxor|bsl|bsr)

% List operators.
LIST_OP                         = (\||\|\||<-|\+\+|--)

% Comparison operators.
COMP_OP                         = (==|/=|=<|<|>=|>|=:=|=/=)

% Process action operators.
PROC_OP                         = (->|\*\*|!|\?)

% Mathematical operators.
MATH_OP                         = (\+|-|\*|/|div|rem)

% Whitespace characters.
WS                              = ([\s\t\n\r]|%.*)


%%% ----------------------------------------------------------------------------
%%% Lexical token rule definitions.
%%% ----------------------------------------------------------------------------

Rules.

&&										: {token, {and_op, TokenLine}}.

% Boolean operator tokens.
{BOOL_OP}                       : {token, {?to_atom(TokenChars), TokenLine}}.

% Bitwise operator tokens.
{BIT_OP}                       : {token, {?to_atom(TokenChars), TokenLine}}.

% List operator tokens.
{LIST_OP}                       : {token, {?to_atom(TokenChars), TokenLine}}.

% Comparison operator tokens.
{COMP_OP}                       : {token, {?to_atom(TokenChars), TokenLine}}.

% Process action operator tokens.
{PROC_OP}                       : {token, {?to_atom(TokenChars), TokenLine}}.

% Mathematical operator tokens.
{MATH_OP}                       : {token, {?to_atom(TokenChars), TokenLine}}.

% Variable token.
{UPPER_}+{ALPHA}*               : {token, {var, TokenLine, ?to_atom(TokenChars)}}.

% Atom and keyword tokens.
{LOWER}{ALPHA}*                 : Atom = ?to_atom(TokenChars),
                                  {token,
                                    case is_keyword(Atom) of
                                      true -> {Atom, TokenLine};
                                      false -> {atom, TokenLine, Atom}
                                    end
                                  }.

% Integer tokens.
{DIGIT}+                        : {token, {integer, TokenLine, ?to_integer(TokenChars)}}.

% Float tokens.
{DIGIT}+\.{DIGIT}+              : {token, {float, TokenLine, ?to_float(TokenChars)}}.

% PID tokens.
<{DIGIT}+\.{DIGIT}+\.{DIGIT}+>  : {token, {pid, TokenLine, ?to_pid(TokenChars)}}.

% String tokens.
"(\\\^.|\\.|[^"])*"             : {token, {string, TokenLine, ?to_string(TokenChars)}}.

% Punctuation and meta symbol tokens.
[()\[\]{}\,\.\;:]               : {token, {?to_atom(TokenChars), TokenLine}}.

% Whitespace and comment tokens.
{WS}+                           : skip_token.


%%% ----------------------------------------------------------------------------
%%% Erlang supporting macros and code.
%%% ----------------------------------------------------------------------------

Erlang code.

%% String conversion macros.
-define(to_atom(String), list_to_atom(String)).
-define(to_integer(String), list_to_integer(String)).
-define(to_float(String), list_to_float(String)).
-define(to_pid(String), list_to_pid(String)).
-define(to_string(String), lists:sublist(String, 2, length(String) - 2)).

%% Generates the string token from the specified list of characters.
string_gen([$\\|Cs]) ->
  string_escape(Cs);
string_gen([C|Cs]) ->
  [C | string_gen(Cs)];
string_gen([]) -> [].

%% Escapes all ASCII control characters in the specified string.
string_escape([O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
  [(O1*8 + O2)*8 + O3 - 73*$0|string_gen(S)];
string_escape([$^,C|Cs]) ->
  [C band 31|string_gen(Cs)];
string_escape([C|Cs]) when C >= $\000, C =< $\s ->
  string_gen(Cs);
string_escape([C|Cs]) ->
  [escape_char(C)|string_gen(Cs)].

%% Escapes the specified ASCII control character.
escape_char($n) -> $\n;        %\n = LF
escape_char($r) -> $\r;        %\r = CR
escape_char($t) -> $\t;        %\t = TAB
escape_char($v) -> $\v;        %\v = VT
escape_char($b) -> $\b;        %\b = BS
escape_char($f) -> $\f;        %\f = FF
escape_char($e) -> $\e;        %\e = ESC
escape_char($s) -> $\s;        %\s = SPC
escape_char($d) -> $\d;        %\d = DEL
escape_char(C) -> C.

% Decentralized monitor keywords.
is_keyword(with) -> true;
is_keyword('when') -> true;
is_keyword(monitor) -> true;

% Normalized Safety HML keywords.
is_keyword(tt) -> true;
is_keyword(ff) -> true;
is_keyword(max) -> true;

% Erlang keywords.

% Others.
is_keyword(_) -> false.
