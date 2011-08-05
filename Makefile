REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)
REBAR_FLAGS ?=

VSN := "0.3.6"
BUILD_DATE := `LANG=C date +"%a %b %d %Y"`
NAME := rtpproxy

ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/$(NAME)-$(VSN)

EBIN_DIR := ebin
ERL_SOURCES  := $(wildcard src/*.erl)
ERL_OBJECTS  := $(ERL_SOURCES:src/%.erl=$(EBIN_DIR)/%.beam)
APP_FILE := $(EBIN_DIR)/$(NAME).app

all: compile

compile:
	VSN=$(VSN) BUILD_DATE=$(BUILD_DATE) $(REBAR) compile $(REBAR_FLAGS)

install: all
	install -D -p -m 0644 $(APP_FILE) $(DESTDIR)$(ERLDIR)/$(APP_FILE)
	install -p -m 0644 $(ERL_OBJECTS) $(DESTDIR)$(ERLDIR)/$(EBIN_DIR)
	install -D -p -m 0644 priv/erlrtpproxy.config $(DESTDIR)/etc/erl$(NAME).config
	install -D -p -m 0755 priv/erlrtpproxy.init $(DESTDIR)/etc/rc.d/init.d/erl$(NAME)
	install -D -p -m 0644 priv/erlrtpproxy.sysconfig $(DESTDIR)/etc/sysconfig/erl$(NAME)
	install -d $(DESTDIR)/var/lib/erl$(NAME)
	install -D -p -m 0644 priv/erlang.cookie $(DESTDIR)/var/lib/erl$(NAME)/.erlang.cookie
	install -D -p -m 0644 priv/hosts.erlang $(DESTDIR)/var/lib/erl$(NAME)/.hosts.erlang

test:
	$(REBAR) eunit $(REBAR_FLAGS)

clean:
	$(REBAR) clean $(REBAR_FLAGS)
