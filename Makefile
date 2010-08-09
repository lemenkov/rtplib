ERLC := erlc
EMULATOR := beam
ERLLIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
VERSION=0.3.1

EBIN_DIR := ./ebin
ERL_SOURCES  := $(wildcard src/*.erl)
ERL_OBJECTS  := $(ERL_SOURCES:src/%.erl=$(EBIN_DIR)/%.beam)

all: $(EBIN_DIR) $(ERL_OBJECTS)

$(EBIN_DIR)/%.$(EMULATOR): ./src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

check: all
	@./test/run $(shell basename `pwd`)

install:
	for i in ebin/*.beam ebin/*.app include/*.hrl; do install -D -p -m 0644 $$i $(prefix)$(ERLLIBDIR)/rtplib-$(VERSION)/$$i ; done

clean:
	rm -f $(ERL_OBJECTS) ebin/*~ src/*~ test/*~ *~
