all:
	@[ -d ./ebin ] || mkdir -p ./ebin
	@cp src/rtpproxy.app.src ebin/rtpproxy.app
	@cp src/ser.app.src ebin/ser.app
	@erl -make

clean:
	@rm -f ./ebin/*.beam ./ebin/*.app priv/*~ src/*~ *~
