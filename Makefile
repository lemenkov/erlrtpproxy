REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)
REBAR_FLAGS ?=

VSN := "0.12.0"
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
	@VSN=$(VSN) BUILD_DATE=$(BUILD_DATE) $(REBAR) compile $(REBAR_FLAGS)

rel: compile
	rm -rf rel/ser
	$(REBAR) generate $(REBAR_FLAGS)

check: test
test: all
	epmd -daemon
	$(REBAR) eunit $(REBAR_FLAGS)

install: all
	@test -d $(DESTDIR)$(ERLDIR)/ebin || mkdir -p $(DESTDIR)$(ERLDIR)/ebin
	@test -d $(DESTDIR)$(prefix)/etc || mkdir -p $(DESTDIR)$(prefix)/etc
	@test -d $(DESTDIR)$(prefix)/var/lib/erl$(NAME) || mkdir -p $(DESTDIR)$(prefix)/var/lib/erl$(NAME)

	@install -p -m 0644 $(APP_FILE) $(DESTDIR)$(ERLDIR)/ebin
	@install -p -m 0644 $(ERL_OBJECTS) $(DESTDIR)$(ERLDIR)/ebin
	@install -p -m 0644 priv/erlrtpproxy.config $(DESTDIR)$(prefix)/etc/erl$(NAME).config
	@install -p -m 0644 priv/erlang.cookie $(DESTDIR)$(prefix)/var/lib/erl$(NAME)/.erlang.cookie
	@install -p -m 0644 priv/hosts.erlang $(DESTDIR)$(prefix)/var/lib/erl$(NAME)/.hosts.erlang
	@install -p -m 0755 priv/erlrtpproxyctl.escript $(DESTDIR)$(prefix)/usr/sbin/erlrtpproxyctl
ifeq ($(UNAME), Darwin)
	@install -p -m 0644 priv/erlrtpproxy.sysconfig $(DESTDIR)$(prefix)/etc/erl$(NAME)
	@echo "erl$(NAME) installed. \n"
else
	@test -d $(DESTDIR)$(prefix)/etc/rc.d/init.d || mkdir -p $(DESTDIR)$(prefix)/etc/rc.d/init.d
	@test -d $(DESTDIR)$(prefix)/etc/sysconfig || mkdir -p $(DESTDIR)$(prefix)/etc/sysconfig
	@install -p -m 0644 priv/erlrtpproxy.sysconfig $(DESTDIR)$(prefix)/etc/sysconfig/erl$(NAME)
	@install -p -m 0755 priv/erlrtpproxy.init $(DESTDIR)$(prefix)/etc/rc.d/init.d/erl$(NAME)
endif

benchmark: bench
bench:
	@escript -n ./priv/benchmark.escript

clean:
	@$(REBAR) clean $(REBAR_FLAGS)

uninstall:
	@if test -d $(ERLDIR); then rm -rf $(ERLDIR); fi
	@echo "$(NAME) uninstalled. \n
