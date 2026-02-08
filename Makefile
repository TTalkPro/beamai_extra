.PHONY: all compile clean shell

# 链接 beamai 子应用到 _build 目录
BEAMAI_DIR := $(realpath ../beamai)
BUILD_DIR := _build/default/lib

all: compile

prepare_beamai:
	@echo "Compiling beamai project..."
	@cd $(BEAMAI_DIR) && rebar3 compile

prepare_deps:
	@echo "Preparing beamai dependencies in _build..."
	@mkdir -p $(BUILD_DIR)
	@rm -rf $(BUILD_DIR)/beamai_core $(BUILD_DIR)/beamai_llm $(BUILD_DIR)/beamai_memory
	@cp -r $(BEAMAI_DIR)/_build/default/lib/beamai_core $(BUILD_DIR)/ 2>/dev/null || echo "beamai_core not found, skipping..."
	@cp -r $(BEAMAI_DIR)/_build/default/lib/beamai_llm $(BUILD_DIR)/ 2>/dev/null || echo "beamai_llm not found, skipping..."
	@cp -r $(BEAMAI_DIR)/_build/default/lib/beamai_memory $(BUILD_DIR)/ 2>/dev/null || echo "beamai_memory not found, skipping..."

compile: prepare_beamai prepare_deps
	rebar3 compile

shell: prepare_beamai prepare_deps
	rebar3 shell

clean:
	rm -rf _build
