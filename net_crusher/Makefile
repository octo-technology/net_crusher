ERL_SOURCES=$(wildcard *.erl)
ERLC_FLAGS=-W # -Ddebug
CONFIG_FILES=$(wildcard config/*)

EBIN_DIR=../ebin
EXBIN_DIR=../exbin
CONFIG_DIR=../ebin/config
ERLC=erlc
ERL=erl
ELIXIRC=elixirc
EMULATOR=beam
EBIN_FILES=$(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
ELIXIR_FILES=$(shell echo $(ERL_SOURCES:%.erl=$(EXBIN_DIR)/ex%.exs) | sed -e 's/exbin\/ex\(.\)/exbin\/ex\U\1/g')
ELIXIR_BIN_FILES=$(ELIXIR_FILES:$(EXBIN_DIR)/%.exs=$(EBIN_DIR)/%.$(EMULATOR))

SCRIPT_SOURCES=$(wildcard *.escript)
SCRIPT_TARGET=$(SCRIPT_SOURCES:%=$(EBIN_DIR)/%)
CONFIG_FILES_TARGET=$(subst config, $(CONFIG_DIR), $(CONFIG_FILES))

all: $(SCRIPT_TARGET) $(CONFIG_DIR) $(EBIN_DIR) compile $(CONFIG_FILES_TARGET)

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

$(ELIXIR_BIN_FILES): $(EBIN_DIR)/%.$(EMULATOR): $(EXBIN_DIR)/%.exs
	$(ELIXIRC) $< -o $(EBIN_DIR)

$(ELIXIR_FILES): $(EXBIN_DIR)
	./generate_all_modules.sh

$(EBIN_DIR)/%.escript: %.escript
	cp $< $@

$(CONFIG_FILES_TARGET): $(CONFIG_FILES)
	cp $< $@

compile: $(EBIN_FILES) $(ELIXIR_BIN_FILES)

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

$(EXBIN_DIR):
	mkdir -p $@

$(CONFIG_DIR):
	mkdir -p $(CONFIG_DIR)

clean:
	rm -rf $(EBIN_DIR)
	rm -rf $(EXBIN_DIR)
	rm -rf $(CONFIG_DIR)
	@rm -f erl_crash.dump
