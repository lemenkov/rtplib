-module(test_utils).

-compile(export_all).

diff(A, B) ->
	diff(<<>>, A, B).

diff(Ret, <<>>, _) ->
	Ret;
diff(Ret, _, <<>>) ->
	Ret;

diff(Ret, <<ByteA:8, RestA/binary>>, <<ByteB:8, RestB/binary>>) ->
	Diff = ByteA - ByteB,
	diff(<<Ret/binary, Diff:8>>, RestA, RestB).
