# detecter-inline
Source code and system for CAV2020 submission







Different property specifications must be separated with a comma (`,'),
with the last one being terminated with a period (`.'). A property
specification must target one function pattern, but may optionally
include optional guards. The guard specification format follows exactly
the one used in Erlang, the only difference being that guard functions
({@eg} `is_pid/2', `is_alive/1', {@etc}) are not supported. Patterns
and guards on binaries, bitstring and maps have yet to be implemented
in the future. In addition, only external function calls may be
targeted {@ie} `Mod:Fun(Args)', with this being the function that is
specified into a {@link erlang:spawn/3}, {@link erlang:spawn/4},
{@etc} calls.

For instance, a property specification that targets the function named
`test' in module `example' taking two parameters `A' and `B' such that
`A' is an integer in the closed interval `[0,10]' is specified as
follows:

```
with
  example:test(A, B) when A >= 0, A =< 10
monitor
  <property in SHMLnf>
```

The property itself ({@ie} `<property in SHMLnf>') is then written
using the grammar defined as follows:

```
<SHML> ::= ff (falsity, an atom)
         | X (recursion variable specified as a standard Erlang variable)
         | max(X. <SHML>) (maximal fix-point to specify recursive loops,
                           comprised of one variable and a sub-formula
                           <SHML>)
         | and(<SHML list>) (a sequence of comma-separated conjuncts where
                             each is itself a sub-formula <SHML> that however
                             must start with a necessity [<ACTION>]<SHML>)
```

      An action in a necessity `[<ACTION>]' can be one of the following:
         {@item A `fork' action, specified as `<VAR_0> -> <VAR_1>, <MFA>';}
        {@item A process `exit' action, specified as `<VAR> ** <CLAUSE>';}
         {@item A process `send' action, specified as `<VAR> ! <CLAUSE>';}
         {@item A process `receive' action, specified as `<VAR> ? <CLAUSE>';}
         {@item A generic user-given action, specified as `<CLAUSE>'.}

       where `<VAR>', `<VAR_0>' and `<VAR_1>' are standard Erlang variables,
       `<CLAUSE>' is a standard Erlang clause, and `<MFA>' is `Mod:Fun(Args)'
       as described above. Actions in necessities can optionally includes
       guards, albeit with the same restrictions outlined above.

The following are some examples of properties:
```
% Parent process P cannot fork a child process C via @{M,F,Args@} @{child,
% init, [Id, StartCnt]@} (_ is used to match StartCnt since one does not care
% about that value), such that the Id assigned to C is negative.
with
example:test(A, B) when A >= 0, A =< 10
monitor
and([P -> C, child:init([Id, _]) when Id < 0]ff).
```

```
% Parent process P cannot fork a child process C via @{M,F,Args@} @{child,
% init, [Id, StartCnt]@} (_ is used to match StartCnt since one does not care
% about that value), only to terminate immediately with reason `aborted'.
with
 example:test(A, B) when A >= 0, A =< 10
monitor
  and([P -> C, child:init([Id, _])] and([P ** aborted]ff)).
```
```
% Server process S can engage in a request-response cycle such that it
% receives a request @{req, A1, A2@} consisting of two integers A1 and A2,
% returning the result of their addition in the response @{resp, AA@}. It
% however cannot return something other than their addition: this is taken
% care of by the second necessity in the second conjunct via the action
% clause @{resp, AA@} when AA =/= A1 + A2.
with
 example:test(A, B) when A >= 0, A =< 10
monitor
  max(X.
    and(
      [S ? @{req, A1, A2@}] and(
        [S ! @{resp, AA@} when AA =/= A1 + A2]ff,
        [S ! @{resp, AA@} when AA =:= A1 + A2]X)
      )
    )
  ).
```








hml_eval:compile("examples/example_1.hml", [{outdir, "ebin"}, v]).

hml_eval:compile("examples/example_1.hml", [{outdir, "ebin"}]).

weaver:weave_file("src/system/server.erl", fun example_1:mfa_spec/1, fun launcher:filter_spec/1, [{outdir, "ebin"}]).

