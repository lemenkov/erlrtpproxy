all:
	erlc rtpproxy.erl
	erlc call.erl
	erlc media.erl
	erlc syslog.erl
	erlc ser.erl

clean:
	rm -f *.beam *~
