ERL_SOURCES=$(wildcard *.erl)
ERLC_FLAGS=-W -o $(EBIN_DIR)

EBIN_DIR=../ebin
ERLC=erlc
ERL=erl
EMULATOR=beam
MKTEMP=mktemp
BEAM=$(SRC:%.erl=%.$(EMULATOR))
EBIN_FILES=$(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
TEST_MODULES=$(ERL_SOURCES:%.erl=%)

.PHONY: test

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

all: test

compile: $(EBIN_DIR) $(EBIN_FILES)

$(EBIN_DIR):
	mkdir -p $@

clean:
	rm -f $(EBIN_FILES)
	@rm -f erl_crash.dump

$(TEST_MODULES):
	@tempfile=`mktemp net_crusher_test`; \
	$(ERL) -pa $(EBIN_DIR) -mnesia schema_location ram -run $@ test -run init stop -noshell | tee $$tempfile; \
	grep failed $$tempfile > /dev/null; \
	result=`[ $$? -eq 0 ] && echo 1 || echo 0`; \
	rm -f $$tempfile; \
	[ $$result -ne 0 ] && exit $$result || true

test: compile $(TEST_MODULES)
	./test.sh
