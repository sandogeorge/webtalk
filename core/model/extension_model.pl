:- module(extension_model,
        [   attach_extension_db/1,   % +File
            detach_extension_db/0,   % -
            sync_extension_db/1,     % +What
            current_extension/1,     % ?Extension
            add_extension/1,         % +Extension
            update_extension/1,      % +Extension
            delete_extension/1       % +Extension
        ]).

:- use_module(library(persistency)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- persistent
    extension_record(name:atom).

attach_extension_db(File) :-
    db_attach(File, []).

detach_extension_db :- db_detach.

sync_extension_db(What) :-
    db_sync(What).

current_extension(Name) :-
    with_mutex(extension_model, extension_record(Name)).

add_extension(Name) :-
    findall(X, current_extension(X), Names),
    \+(member(Name, Names)),
    assert_extension_record(Name).

update_extension(Name) :-
    with_mutex(extension_model,
                (   retractall_extension_record(Name),
                    assert_extension_record(Name))).

delete_extension(Name) :-
    with_mutex(extension_model,
                retractall_extension_record(Name)).