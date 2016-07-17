-module(breduce_untyped_lc).
-behaviour(breduce_lc).
-export([parse/1, format/1, check/1, breduce/1]).

parse(_Text) -> 0 = 1.

format(_Term) -> 0 = 1.

check(Term) -> {ok, Term}.

breduce(Term) -> breduce(Term, #{}).

breduce(Term = {var, Name}, Env) ->
    maps:get(Name, Env, Term);
breduce({abs, Param, Body}, Env) ->
    % The reason we create BodyEnv, as opposed to just passing Env, is that we
    % might need to shadow the outer Param. Consider "(\x. \x. x) a". This must
    % reduce to "\x. x", not to "\x. a".
    BodyEnv = maps:put(Param, {var, Param}, Env),
    {abs, Param, breduce(Body, BodyEnv)};
breduce({app, Func, Arg}, Env) ->
    Func2 = breduce(Func, Env),
    Arg2 = breduce(Arg, Env),
    case Func2 of
        {abs, Param, Body} ->
            BodyEnv = maps:put(Param, Arg2, Env),
            breduce(Body, BodyEnv);
        _ -> {app, Func2, Arg2}
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

breduce_test() ->
    ?assert(breduce({var, "x"}) =:= {var, "x"}),
    ?assert(breduce({app, {var, "f"}, {var, "x"}}) =:= {app, {var, "f"}, {var, "x"}}),
    ?assert(breduce({app, {abs, "x", {var, "x"}}, {var, "a"}}) =:= {var, "a"}),
    ?assert(breduce({app, {abs, "x", {abs, "x", {var, "x"}}}, {var, "a"}}) =:= {abs, "x", {var, "x"}}),
    ok.

-endif.
