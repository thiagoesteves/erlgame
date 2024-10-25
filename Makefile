.PHONY: run test clean compile release check coveralls
	
run:
	rebar3 shell --apps erlgame
	
test:
	rebar3 as test ct --cover && rebar3 as test cover --verbose
	
clean:
	rebar3 clean
	
compile:
	rebar3 as prod compile
	
release: compile
	rebar3 as prod release
	
check:
	rebar3 as test dialyzer
	
coveralls: test
	rebar3 as test coveralls send
