-module(breduce_lc).

-callback parse(Text :: string()) -> {ok, term()} | {error, term()}.
-callback format(Term :: term()) -> string().
-callback check(Term :: term()) -> {ok, term()} | {error, term()}.
-callback breduce(Term :: term()) -> term().
