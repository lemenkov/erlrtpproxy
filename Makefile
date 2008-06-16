all:
	erlc rtpproxy.erl
	erlc call.erl
	erlc media.erl
	erlc syslog.erl

clean:
	rm -f *.beam *~
