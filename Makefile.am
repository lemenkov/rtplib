SUBDIRS = c_src

all:
	$(mkdir_p) ./priv/
	$(install_sh_PROGRAM) c_src/.libs/*.so ./priv/
	@VSN="@PACKAGE_VERSION@" $(REBAR) get-deps
	@VSN="@PACKAGE_VERSION@" $(REBAR) compile
check:
	$(mkdir_p) ./priv/
	$(install_sh_PROGRAM) c_src/.libs/*.so ./priv/
	@$(REBAR) eunit

clean-local:
	@rm -f priv/*.so priv/erlang-rtplib.spec
	@$(REBAR) delete-deps
	@$(REBAR) clean

install-exec-local:
	$(mkdir_p) $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/ebin/
	$(install_sh_DATA) ebin/*.beam ebin/rtplib.app $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/ebin/
	$(mkdir_p) $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/include/
	$(install_sh_DATA) include/*.hrl $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/include/

uninstall-local:
	@rm $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/ebin/*.beam
	@rm $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/ebin/rtplib.app
	@rm $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/include/*.hrl
	@rm -r $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/ebin/
	@rm -r $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/include/
	@rm -r $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/

EXTRA_DIST = include/*.hrl src/*.erl src/rtplib.app.src test/samples test/*.erl rebar rebar.config
