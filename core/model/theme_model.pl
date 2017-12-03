:- module(theme_model,
        [   attach_theme_db/1,   % +File
            detach_theme_db/0,   % -
            sync_theme_db/1,     % +What
            current_theme/2,     % ?Theme, ?IsDefault
            add_theme/2,         % +Theme, +IsDefault
            update_theme/2,      % +Theme, +IsDefault
            delete_theme/1       % +Theme
        ]).

:- use_module(library(persistency)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- persistent
    theme_record(name:atom, is_default:atom).

attach_theme_db(File) :-
    db_attach(File, []).

detach_theme_db :- db_detach.

sync_theme_db(What) :-
    db_sync(What).

current_theme(Name, IsDefault) :-
    with_mutex(theme_model, theme_record(Name, IsDefault)).

add_theme(Name, IsDefault) :-
    findall(X, current_theme(X, _), Names),
    \+(member(Name, Names)),
    assert_theme_record(Name, IsDefault).

update_theme(Name, IsDefault) :-
    with_mutex(theme_model,
                (   retractall_theme_record(Name, _),
                    assert_theme_record(Name, IsDefault))).

delete_theme(Name) :-
    with_mutex(theme_model,
                retractall_theme_record(Name, _)).