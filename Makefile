VSN := 0.3
BUILD_DATE := `LANG=C date +"%a %b %d %Y"`
NAME := ser

ERLC := /usr/bin/erlc
ERLC_FLAGS := +debug_info
EMULATOR := beam
ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/$(NAME)-$(VSN)

EBIN_DIR := ebin
ERL_SOURCES  := $(wildcard src/*.erl)
ERL_OBJECTS  := $(ERL_SOURCES:src/%.erl=$(EBIN_DIR)/%.beam)
APP_FILE := $(EBIN_DIR)/$(NAME).app
SPEC_FILE := priv/erlrtpproxy-ser.spec

all: $(EBIN_DIR) $(ERL_OBJECTS) $(APP_FILE) $(SPEC_FILE)

$(EBIN_DIR)/%.$(EMULATOR): ./src/%.erl
	$(ERLC) $(ERLC_FLAGS) -I include -o $(EBIN_DIR) $<

$(EBIN_DIR)/%.app: ./src/%.app.src
	sed -e "s,%VSN%,$(VSN),g" $< > $@

priv/%.spec: priv/%.spec.in
	sed -e "s,%VSN%,$(VSN),g;s,%DATE%,$(BUILD_DATE),g"  $< > $@

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

install: all
	install -D -p -m 0644 $(APP_FILE) $(DESTDIR)$(ERLDIR)/$(APP_FILE)
	install -p -m 0644 $(ERL_OBJECTS) $(DESTDIR)$(ERLDIR)/$(EBIN_DIR)
	install -D -p -m 0644 priv/erlrtpproxy-ser.config $(DESTDIR)/etc/$(NAME).config
	install -D -p -m 0755 priv/erlrtpproxy-ser.init $(DESTDIR)/etc/rc.d/init.d/$(NAME)
	install -D -p -m 0644 priv/erlrtpproxy-ser.sysconfig $(DESTDIR)/etc/sysconfig/$(NAME)

clean:
	rm -f $(ERL_OBJECTS) $(APP_FILE) $(SPEC_FILE) priv/*~ src/*~ *~
