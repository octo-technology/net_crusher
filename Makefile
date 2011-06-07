ERL_SOURCES=$(wildcard *.erl)
ERLC_FLAGS=-W -o $(EBIN_DIR)

SUBDIRS=common json net_crusher monitoring

EBIN_DIR=ebin
ERLC=erlc
ERL=erl
EMULATOR=beam
BEAM=$(SRC:%.erl=%.$(EMULATOR))
EBIN_FILES=$(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))

.PHONY: $(SUBDIRS) test

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

all: compile

compile: ebin $(EBIN_FILES) $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

ebin:
	mkdir -p $(EBIN_DIR)

clean:
	rm -rf $(EBIN_DIR)
	@rm -f erl_crash.dump

erl: all
	$(ERL) -pz $(EBIN_DIR)

test: all
	$(MAKE) -C $@
