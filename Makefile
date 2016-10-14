ERL=erl
BEAMDIR=./deps/*/ebin ./ebin
BASE_DIR=$(shell pwd)
REBAR = $(BASE_DIR)/rebar

all: clean get-deps compile
update-deps:
	@$(REBAR) update-deps

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

edoc:
	@$(REBAR) doc skip_deps=true

clean:
	@$(REBAR) clean

xref:
	@$(REBAR) xref skip_deps=true

rel: compile
	@cd rel && $(REBAR) generate -f

tests: clean get-deps compile eunit ct

eunit:
	# @rm -rf logs
	$(REBAR) eunit skip_deps=true

ct:
	@$(REBAR) ct skip_deps=true
