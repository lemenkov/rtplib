SUBDIRS = c_src

all:
	@cp -av c_src/.libs/*.so ./priv/
	@VSN="@PACKAGE_VERSION@" ./rebar compile
check:
	@cp -av c_src/.libs/*.so ./priv/
	@./rebar eunit

clean-local:
	@rm -f priv/*.so priv/erlang-rtplib.spec
	@./rebar clean
