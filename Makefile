REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)
REBAR_FLAGS ?=

VSN := "0.4.3"
BUILD_DATE := `LANG=C date +"%a %b %d %Y"`
NAME := rtplib

ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/$(NAME)-$(VSN)

EBIN_DIR := ebin
ERL_SOURCES  := $(wildcard src/*.erl)
ERL_OBJECTS  := $(ERL_SOURCES:src/%.erl=$(EBIN_DIR)/%.beam)
APP_FILE := $(EBIN_DIR)/$(NAME).app

all: compile

compile:
	VSN=$(VSN) BUILD_DATE=$(BUILD_DATE) $(REBAR) compile $(REBAR_FLAGS)

check: all
	@./test/run $(shell basename `pwd`)

install: all
	for i in ebin/*.beam ebin/*.app include/*.hrl; do install -D -p -m 0644 $$i $(DESTDIR)$(ERLDIR)/$$i ; done

clean:
	$(REBAR) clean $(REBAR_FLAGS)
