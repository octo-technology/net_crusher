ERL_SOURCES=$(wildcard *.erl)
ERLC_FLAGS=-W -o $(EBIN_DIR)

EBIN_DIR=../ebin
ERLC=erlc
EMULATOR=beam
BEAM=$(SRC:%.erl=%.$(EMULATOR))
EBIN_FILES=$(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

all: compile 

compile: $(EBIN_DIR) $(EBIN_FILES)

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

clean:
	rm -rf $(EBIN_DIR)
	@rm -f erl_crash.dump
