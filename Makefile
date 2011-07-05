all:
	@[ -d ./ebin ] || mkdir -p ./ebin
	@cp src/rtpproxy.app.src ebin/rtpproxy.app
	@erl -make

clean:
	@rm -f ./ebin/*.beam ./ebin/*.app priv/*~ src/*~ *~
