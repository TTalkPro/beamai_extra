.PHONY: compile shell clean

ERL_LIBS := ../_build/default/lib:../_build/default/lib/beamai_core/apps:../_build/default/lib/beamai_llm/apps:../_build/default/lib/beamai_memory/apps

compile:
	ERL_LIBS=$(ERL_LIBS) rebar3 compile

shell:
	ERL_LIBS=$(ERL_LIBS) rebar3 shell

clean:
	rm -rf _build rebar.lock
