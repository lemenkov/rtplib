VSN := 0.3.1
BUILD_DATE := `LANG=C date +"%a %b %d %Y"`
NAME := rtplib

ERLC := erlc
ERLC_FLAGS := +debug_info
EMULATOR := beam
ERLLIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)

EBIN_DIR := ./ebin
ERL_SOURCES  := $(wildcard src/*.erl)
ERL_OBJECTS  := $(ERL_SOURCES:src/%.erl=$(EBIN_DIR)/%.beam)
APP_FILE := $(EBIN_DIR)/$(NAME).app

all: $(EBIN_DIR) $(ERL_OBJECTS) $(APP_FILE)

$(EBIN_DIR)/%.$(EMULATOR): ./src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

$(EBIN_DIR)/%.app: ./src/%.app.src
	sed -e "s,%VSN%,$(VSN),g" $< > $@

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

check: all
	@./test/run $(shell basename `pwd`)

install:
	for i in ebin/*.beam ebin/*.app include/*.hrl; do install -D -p -m 0644 $$i $(DESTDIR)$(ERLLIBDIR)/$(NAME)-$(VSN)/$$i ; done

clean:
	rm -f $(ERL_OBJECTS) $(APP_FILE)  src/*~ test/*~ *~
