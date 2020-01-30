################################################################################
## Emulation configurations                                                   ##
################################################################################


# Result = weaver:weave("src/models", "include", "ebin2", fun({slave, init, [Id, Chunks]}) -> {ok, fun Mon(Event) -> Mon end}; (_) -> undefined end).

# weaver:weave_file("src/main.erl", "include", "ebin", fun({slave, init, [Id, Chunks]}) -> {ok, fun Mon(Event) -> Mon end}; (_) -> undefined end, fun(_) -> true end).

################################################################################
## Project configurations                                                     ##
################################################################################

BIN=ebin
INCLUDE=include
SRC=src
TEST=test

SCRIPTS=scripts
VENV=$(SCRIPTS)/venv

# Set shell to bash to use certain commands such as source.
SHELL=/bin/bash

define recursive
	$(shell find $(1) -name "*.$(2)")
endef

all: compile

compile: clean
	mkdir -p $(BIN)
	erlc -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl)

# compile-test: clean
# 	mkdir -p $(BIN)
# 	erlc -DTEST -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl)
# 	erlc -DTEST -pa $(BIN) -I $(INCLUDE) -o $(BIN) $(call recursive,$(TEST),erl)
#
# test: compile-test
# 	erl -noshell -pa ebin -eval 'eunit:test(log_tracer_test, [verbose])' -s init stop
# 	erl -noshell -pa ebin -eval 'eunit:test(async_tracer_test, [verbose])' -s init stop

weave: compile
	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/system", "$(INCLUDE)", "$(BIN)", fun launcher:driver_monitors/1, fun launcher:filter_spec/1).' -s init stop

run:
	mkdir -p $(LOG_DIR)
	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -noshell -eval '$(APP_CMD)' -s init stop
# 	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -noshell -eval '$(APP_CMD)'
# 	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -eval '$(APP_CMD)'

clean:
	rm -rf $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.tmp erl_crash.dump
