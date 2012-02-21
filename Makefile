REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)
REBAR_FLAGS ?=

VSN := "0.5.7"
BUILD_DATE := `LANG=C date +"%a %b %d %Y"`
NAME := rtplib
UNAME := $(shell uname -s)

ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/$(NAME)-$(VSN)

EBIN_DIR := ebin
ERL_SOURCES  := $(wildcard src/*.erl)
ERL_OBJECTS  := $(ERL_SOURCES:src/%.erl=$(EBIN_DIR)/%.beam)
APP_FILE := $(EBIN_DIR)/$(NAME).app

all: compile

compile:
	@VSN=$(VSN) BUILD_DATE=$(BUILD_DATE) $(REBAR) compile $(REBAR_FLAGS)

check: test
test: all
	@rm -rf .eunit
	$(REBAR) eunit

install: all
ifeq ($(UNAME), Darwin)
	@test -d $(DESTDIR)$(ERLDIR) || mkdir -p $(DESTDIR)$(ERLDIR)/{$(EBIN_DIR),include,priv}
	@install -p -m 0644 ebin/*.beam $(DESTDIR)$(ERLDIR)/$(EBIN_DIR)
	@install -p -m 0644 ebin/*.app $(DESTDIR)$(ERLDIR)/$(EBIN_DIR)
	@install -p -m 0644 include/*.hrl $(DESTDIR)$(ERLDIR)/include
	@install -p -m 0755 priv/*.so $(DESTDIR)$(ERLDIR)/priv
	@echo "\n $(NAME) installed. \n"
else
	for i in ebin/*.beam ebin/*.app include/*.hrl; do install -D -p -m 0644 $$i $(DESTDIR)$(ERLDIR)/$$i ; done
	for i in priv/*.so; do install -D -p -m 0755 $$i $(DESTDIR)$(ERLDIR)/$$i ; done
endif

clean:
	@$(REBAR) clean $(REBAR_FLAGS)

uninstall:
	@if test -d $(ERLDIR); then rm -rf $(ERLDIR); fi
	@echo "$(NAME) uninstalled. \n
