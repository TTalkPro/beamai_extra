.PHONY: compile shell clean

# checkouts 目录用于本地 _checkouts 开发（符号链接到 beamai 工作副本）；
# 无 _checkouts 时 git 依赖落在 lib 下，该路径不存在也无害。
ERL_LIBS := ../_build/default/lib:../_build/default/checkouts

compile:
	ERL_LIBS=$(ERL_LIBS) rebar3 compile

shell:
	ERL_LIBS=$(ERL_LIBS) rebar3 shell

clean:
	rm -rf _build rebar.lock
