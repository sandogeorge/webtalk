:- object(api).
    :- public(index/1).
    index(_Request) :-
        ::write_json(_{
            status: "OK"
        }).

    :- private(write_json/1).
    write_json(Data) :-
        format('Content-type: application/json~n~n'),
        current_output(Out),
        http_json:json_write(Out, Data).
:- end_object.