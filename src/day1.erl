-module(day1).
-export([run/0]).

run() ->
    code:priv_dir(advent2017) ++ module:name().
