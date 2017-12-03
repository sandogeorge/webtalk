:- module(extension_model,
        [   attach_extension_db/1,   % +File
            detach_extension_db/0,   % -
            sync_extension_db/1,     % +What
            current_extension/2,     % ?Extension, ?Required
            add_extension/2,         % +Extension, +Required
            update_extension/2,      % +Extension, +Required
            delete_extension/1       % +Extension
        ]).

:- use_module(library(persistency)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- persistent
    extension_record(name:atom, required:atom).

attach_extension_db(File) :-
    db_attach(File, []).

detach_extension_db :- db_detach.

sync_extension_db(What) :-
    db_sync(What).

current_extension(Name, Required) :-
    with_mutex(extension_model, extension_record(Name, Required)).

add_extension(Name, Required) :-
    findall(X, current_extension(X, _), Names),
    \+(member(Name, Names)),
    assert_extension_record(Name, Required).

update_extension(Name, Required) :-
    with_mutex(extension_model,
                (   retractall_extension_record(Name, _),
                    assert_extension_record(Name, Required))).

delete_extension(Name) :-
    with_mutex(extension_model,
                retractall_extension_record(Name, _)).