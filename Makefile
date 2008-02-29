all:
	erlc rtpproxy.erl
	erlc rtpsocket.erl
	erlc call.erl

clean:
	rm -f *~ *.beam

