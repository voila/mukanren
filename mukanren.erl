%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% https://github.com/jasonhemann/microKanren/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(mukanren).
-compile(export_all).

%% empty state
empty() -> {[], 0}.

assp(_, []) -> false;
assp(F, [{X,V}|Rest]) ->
    case F(X) of
        false -> assp(F, Rest);
        _  -> {X,V}
    end.

var({var, _}) -> true;
var(_) -> false.

veq(X,X) -> true;
veq(_,_) -> false.

walk(U, Subst) ->
    Pr = var(U) andalso assp(fun(V) -> veq(U,V) end, Subst),
    case Pr of
        {_,Val} -> walk(Val, Subst);
        false -> U
    end.

ext_subst(K, V, Subst) ->
    [{K,V}|Subst].


equiv(U,V) ->
    fun({Subst0, Count}) ->
            Subst = unify(U,V,Subst0),     
            case Subst of 
                false -> mzero();
                _ -> unit({Subst,Count})
            end
    end.

unify(U0,V0,Subst0) ->
    U = walk(U0,Subst0),
    V = walk(V0,Subst0),
    case var(U) andalso var(V) andalso veq(U,V) of true -> Subst0;
        false ->  
            case var(U) of true -> ext_subst(U,V,Subst0);  
                false ->
                    case var(V) of true -> ext_subst(V,U,Subst0);  
                        false ->
                            case {U,V} of 
                                {{U1,U2},{V1,V2}} -> 
                                    Subst = unify(U1,V1, Subst0),
                                    Subst andalso unify(U2,V2,Subst);  
                                _ -> U == V andalso Subst0
                            end
                    end
            end
    end.

call_fresh(F) ->
    fun({Subst,Count}) ->
            (F({var,Count}))({Subst, Count+1})
    end.


disj(G1,G2) ->
    fun({_Subst,_Count}=State) ->
            mplus(G1(State), G2(State))
    end.

conj(G1,G2) ->
    fun({_Subst,_Count}=State) ->
            bind(G1(State), G2)
    end.


%% stream monad
unit(State) -> [State|mzero()].

bind([], _G) -> mzero();
bind(S, G) when is_function(S) -> 
    fun() -> bind(S(), G) end;
bind([H|T],G) -> 
    mplus(G(H), bind(T,G)).

mzero() -> [].

mplus([], S2) -> S2;
mplus(S1, S2) when is_function(S1) -> 
    fun() -> mplus(S2, S1()) end;
mplus([H|T],S2) -> 
    [H|mplus(T,S2)].



