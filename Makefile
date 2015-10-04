.PHONY: test all

all:
	@rebar3 compile
test:
	@rebar3 eunit -v
