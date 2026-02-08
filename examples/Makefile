.PHONY: compile shell clean

ERL_LIBS := ../_build/default/lib

compile:
	ERL_LIBS=$(ERL_LIBS) rebar3 compile

shell:
	ERL_LIBS=$(ERL_LIBS) rebar3 shell

clean:
	rm -rf _build rebar.lock
