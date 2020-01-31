# detecter-inline
Source code and system for CAV2020 submission




hml_eval:compile("examples/example_1.hml", [{outdir, "ebin"}, v]).

hml_eval:compile("examples/example_1.hml", [{outdir, "ebin"}]).

weaver:weave_file("src/system/server.erl", fun example_1:mfa_spec/1, fun launcher:filter_spec/1, [{outdir, "ebin"}]).

