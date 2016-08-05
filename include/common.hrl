%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%      erlang常用接口封装
%%% @end
%%% Created : 29. 七月 2016 12:01
%%%-------------------------------------------------------------------

-define(IF(Expression, TrueExp, FalseExp),
    case Expression of
        true ->
            TrueExp;
        false ->
            FalseExp
    end).


-define(INFO(String, Format), io:format(String, Format)).
-define(INFO(String), io:format(String)).

-define(ERROR(String, Format), io:format(String, Format)).
-define(ERROR(String), io:format(String)).