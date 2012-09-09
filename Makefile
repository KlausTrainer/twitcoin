REBAR:=$(shell which rebar || echo ./rebar)

all: get-deps compile-all

get-deps:
	@$(REBAR) get-deps

compile-all:
	@$(REBAR) compile

compile:
	@$(REBAR) skip_deps=true compile

check: compile
	@$(REBAR) skip_deps=true ct

dialyze: compile
	dialyzer ebin

doc:
	@$(REBAR) doc

clean:
	@rm -rf test/*.beam
	@rm -rf erl_crash.dump
	@$(REBAR) clean
