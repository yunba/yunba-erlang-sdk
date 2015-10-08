.PHONY: test

ERL=erl
BEAMDIR=./deps/*/ebin ./ebin
REBAR=./rebar
REBAR_GEN=../../rebar
DIALYZER=dialyzer

#update-deps 
all: deps compile

deps:
	@$(REBAR) get-deps

update-deps:
	@$(REBAR) update-deps

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref skip_deps=true

clean:
	@$(REBAR) clean

test:
	$(REBAR) compile ct skip_deps=true

edoc:
	@$(REBAR) doc

dialyzer: compile
	@$(DIALYZER) ebin deps/ossp_uuid/ebin

setup-dialyzer:
	@$(DIALYZER) --build_plt --apps kernel stdlib mnesia eunit erts crypto
