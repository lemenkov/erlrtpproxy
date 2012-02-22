#!/bin/bash
[ -f ~/.hosts.erlang ] || touch ~/.hosts.erlang
erl +K true +W w -name erlrtpproxy@powerbook.local -rsh ssh -config /opt/local/etc/erlrtpproxy.config -s rtpproxy_ctl
