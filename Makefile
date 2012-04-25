REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)
REBAR_FLAGS ?=

VSN := "0.0.11"
BUILD_DATE := `LANG=C date +"%a %b %d %Y"`
NAME := rtpproxy_notifier

ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/$(NAME)-$(VSN)

EBIN_DIR := ebin
ERL_SOURCES  := $(wildcard src/*.erl)
ERL_OBJECTS  := $(ERL_SOURCES:src/%.erl=ebin/%.beam)
APP_FILE := ebin/$(NAME).app

all: compile

compile:
	VSN=$(VSN) BUILD_DATE=$(BUILD_DATE) $(REBAR) compile $(REBAR_FLAGS)

install: all
	test -d $(DESTDIR)$(ERLDIR)/ebin || mkdir -p $(DESTDIR)$(ERLDIR)/ebin
	test -d $(DESTDIR)$(prefix)/etc || mkdir -p $(DESTDIR)$(prefix)/etc
	install -p -m 0644 $(APP_FILE) $(DESTDIR)$(ERLDIR)/ebin
	install -p -m 0644 $(ERL_OBJECTS) $(DESTDIR)$(ERLDIR)/ebin
	install -p -m 0644 priv/erl$(NAME).config $(DESTDIR)/etc

test:
	$(REBAR) eunit $(REBAR_FLAGS)

clean:
	$(REBAR) clean $(REBAR_FLAGS)
