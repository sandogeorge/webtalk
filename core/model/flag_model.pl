:- module(flag_model,
        [   attach_flag_db/1,   % +File
            detach_flag_db/0,   % -
            sync_flag_db/1,     % +What
            current_flag/2,     % ?Flag, ?Value
            add_flag/2,         % +Flag, +Value
            update_flag/2,      % +Flag, +Value
            delete_flag/1       % +Flag
        ]).

:- use_module(library(persistency)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- persistent
    flag_record(name:atom, value:atom).

attach_flag_db(File) :-
    db_attach(File, []).

detach_flag_db :- db_detach.

sync_flag_db(What) :-
    db_sync(What).

current_flag(Name, Value) :-
    with_mutex(flag_model, flag_record(Name, Value)).

add_flag(Name, Value) :-
    findall(X, current_flag(X, _), Names),
    \+(member(Name, Names)),
    assert_flag_record(Name, Value).

update_flag(Name, Value) :-
    with_mutex(flag_model,
                (   retractall_flag_record(Name, _),
                    assert_flag_record(Name, Value))).

delete_flag(Name) :-
    with_mutex(flag_model,
                retractall_flag_record(Name, _)).