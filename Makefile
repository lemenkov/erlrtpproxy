ERLC := /usr/bin/erlc
EMULATOR := beam

EBIN_DIR := ./ebin
ERL_SOURCES  := $(wildcard src/*.erl)
ERL_OBJECTS  := $(ERL_SOURCES:src/%.erl=$(EBIN_DIR)/%.beam)
ERLC_FLAGS := +debug_info

all: $(EBIN_DIR) $(ERL_OBJECTS)

$(EBIN_DIR)/%.$(EMULATOR): ./src/%.erl
	$(ERLC) $(ERLC_FLAGS) -I include -o $(EBIN_DIR) $<

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

clean:
	rm -f $(ERL_OBJECTS) priv/*~ src/*~ *~
