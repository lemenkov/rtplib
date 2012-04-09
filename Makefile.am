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

install-exec-local:
	$(MKDIR_P) $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/ebin/
	$(install_sh_DATA) ebin/*.beam ebin/rtplib.app $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/ebin/
	$(MKDIR_P) $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/include/
	$(install_sh_DATA) include/*.hrl $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/include/


EXTRA_DIST = priv/erlang-rtplib-0001-Disable-Opus-on-RHEL5.patch include/*.hrl src/*.erl src/rtplib.app.src test/samples test/*.erl rebar rebar.config
