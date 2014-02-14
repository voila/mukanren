-module(mukanren_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-import(microkanren, [call_fresh/1, disj/2, conj/2, equiv/2, empty/0]).


a_and_b() -> 
    conj(
      call_fresh(fun(A) -> equiv(A,7) end),
      call_fresh(fun(B) -> disj(equiv(B,5), equiv(B,6)) end)).

all_test_() ->
     [?_assert((call_fresh(fun(Q) -> equiv(Q,5) end))(empty()) =:= 
                   [{[{{var,0},5}],1}]),
      ?_assert((a_and_b()) (empty()) =:= 
                   [{[{{var,1},5},{{var,0},7}],2},
                    {[{{var,1},6},{{var,0},7}],2}])].

