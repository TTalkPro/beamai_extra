.PHONY: compile shell clean deps test

# checkouts 目录用于本地 _checkouts 开发（符号链接到 beamai 工作副本）；
# 无 _checkouts 时 git 依赖落在 lib 下，该路径不存在也无害。
ERL_LIBS := ../_build/default/lib:../_build/default/lib/beamai_core/apps:../_build/default/lib/beamai_llm/apps:../_build/default/lib/beamai_agent/apps:../_build/default/checkouts

# 先编上层再编 examples：ERL_LIBS 指向的是上层 _build/default，而 `rebar3 eunit`
# 只更新 _build/test —— 只跑过测试就来跑例子，例子加载的是**上一次** compile 留下
# 的旧 beam。症状极隐蔽：代码明明改了，例子的行为却是旧的。
deps:
	cd .. && rebar3 compile

compile: deps
	ERL_LIBS=$(ERL_LIBS) rebar3 compile

test: deps
	ERL_LIBS=$(ERL_LIBS) rebar3 eunit

shell:
	ERL_LIBS=$(ERL_LIBS) rebar3 shell

clean:
	rm -rf _build rebar.lock
