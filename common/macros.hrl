-ifdef(debug).
-define(TIME(Func, Trace),
        Start = tools:micro_timestamp(),
        Result = Func,
        End = tools:micro_timestamp(),
        io:format("[DEBUG] ~p took ~p~n", [Trace, End - Start]),
        Result).
-else.
-define(TIME(Func, _), Func).
-endif.
