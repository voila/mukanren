-module(mukanren_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-import(microkanren, [call_fresh/1, disj/2, conj/2, equiv/2, empty/0]).


a_and_b() -> 
    conj(
      call_fresh(fun(A) -> equiv(A,7) end),
      call_fresh(fun(B) -> disj(equiv(B,5), equiv(B,6)) end)).



fives(X) ->
    disj(equiv(X,5),
         fun(Ac) -> fun() -> (fives(X))(Ac) end end).

take([H|T], N) when N > 0 ->
    [H|take(T, N-1)];
take(_, 0) -> [].


%% (define appendo
%%   (lambda (l s out)
%%     (disj
%%      (conj (== '() l) (== s out))
%%      (call/fresh
%%       (lambda (a)
%%         (call/fresh
%%          (lambda (d)
%%            (conj
%%             (== `(,a . ,d) l)
%%             (call/fresh
%%              (lambda (res)
%%                (conj
%%                 (== `(,a . ,res) out)
%%                 (lambda (s/c)
%%                   (lambda ()
%%                     ((appendo d s res) s/c))))))))))))))


appendo(L, S, Out) ->
    disj(
      conj(equiv([],L), equiv(S,Out)),
      call_fresh(
        fun(A) ->
           call_fresh(
              fun(D) -> 
                 conj(equiv([A|D],L),
                 call_fresh(
                   fun(Res) -> 
                      conj(equiv([A|Res], Out),
                         fun(Sc) -> 
                            fun() -> (appendo(D,S,Res))(Sc) end
                         end)
                   end))    
              end)
        end)).

%% (define ground-appendo (appendo '(a) '(b) '(a b)))

ground_appendo() ->
    appendo([a],[b],[a,b]).

all_test_() ->
     [%% q == 5
      ?_assert((call_fresh(fun(Q) -> equiv(Q,5) end))(empty()) =:= 
                   [{[{{var,0},5}],1}]),
      %% a and b
      ?_assert((a_and_b()) (empty()) =:= 
                   [{[{{var,1},5},{{var,0},7}],2},
                    {[{{var,1},6},{{var,0},7}],2}]),
      %% who cares
      ?_assert(take((call_fresh(fun(Q) -> fives(Q) end))(empty()), 1) =:= 
                    [{[{{var,0},5}],1}]),

      %% ground appendo
      ?_assert((ground_appendo())(empty()) 
                   ), %% (((#(2) b) (#(1)) (#(0) . a)) . 3))

      %% ((call/fresh (lambda (a) (== (var a) '(2)))) empty-s) ==> '((((#(#(0)) 2)) . 1))
      %% ((call/fresh (lambda (a) (== a '(2)))) empty-s) ==> '((((#(0) 2)) . 1))
      ?_assert(
         (call_fresh(fun(A) -> equiv(A, [2]) end))(empty()) =:= [{[{{var,0},[2]}],1}]
        )


].
