################################################################################
## Emulation configurations                                                   ##
################################################################################

################################################################################
## Project configurations                                                     ##
################################################################################

BIN=ebin
INCLUDE=include
SRC=src
TEST=test

SCRIPTS=scripts

# Set shell to bash to use certain commands such as source.
SHELL=/bin/bash

define recursive
	$(shell find $(1) -name "*.$(2)")
endef

all: compile

compile: clean
	mkdir -p $(BIN)
	erlc -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl)

run:
	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -eval 'code:ensure_modules_loaded([hml_eval, hml_lexer, hml_parser, build, events, idler, log, monitor, opts, util, weaver]).'
clean:
	rm -rf $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.tmp $(BIN)/*.erl erl_crash.dump
