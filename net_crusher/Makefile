ERL_SOURCES=$(wildcard *.erl)
ERLC_FLAGS=-W # -Ddebug
CONFIG_FILES=$(wildcard config/*)

EBIN_DIR=../ebin
CONFIG_DIR=../ebin/config
ERLC=erlc
ERL=erl
EMULATOR=beam
BEAM=$(SRC:%.erl=%.$(EMULATOR))
EBIN_FILES=$(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))

SCRIPT_SOURCES=$(wildcard *.escript *.rb)
SCRIPT_TARGET=$(SCRIPT_SOURCES:%=$(EBIN_DIR)/%)
CONFIG_FILES_TARGET=$(subst config, $(CONFIG_DIR), $(CONFIG_FILES))

all: $(SCRIPT_TARGET) $(CONFIG_DIR) $(EBIN_DIR) compile $(CONFIG_FILES_TARGET)

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

$(EBIN_DIR)/%.escript: %.escript
	cp $< $@

$(EBIN_DIR)/%.rb: %.rb
	cp $< $@

$(CONFIG_FILES_TARGET): $(CONFIG_FILES)
	cp $< $@

compile: $(EBIN_FILES) $(EBIN_DIR)/ruby.beam

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

$(CONFIG_DIR):
	mkdir -p $(CONFIG_DIR)

clean:
	rm -rf $(EBIN_DIR)
	rm -rf $(CONFIG_DIR)
	@rm -f erl_crash.dump

$(EBIN_DIR)/ruby.beam: $(EBIN_FILES) ruby.yrl.tpl
	./generate_grammar.escript $(EBIN_DIR) $(EBIN_DIR) runtime vars stats logger misc statment http_cmd http_static assert tlfj tlfj_vdf distributed file_loader sampler
