ERLC := erlc
EMULATOR := beam

EBIN_DIR := ./ebin
ERL_SOURCES  := $(wildcard src/*.erl)
ERL_OBJECTS  := $(ERL_SOURCES:src/%.erl=$(EBIN_DIR)/%.beam)

all: $(EBIN_DIR) $(ERL_OBJECTS)

$(EBIN_DIR)/%.$(EMULATOR): ./src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

clean:
	rm -f $(ERL_OBJECTS) src/*~ *~
