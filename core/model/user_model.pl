:- module(user_model,
        [   attach_user_db/1,   % +File
            detach_user_db/0,   % -
            sync_user_db/1,     % +What
            current_user/4,     % ?User, ?Email, ?Role
            add_user/4,         % +User, +Email, +Role
            update_user/4,      % +User, +Email, +Role
            delete_user/1       % +User
        ]).

:- use_module(library(persistency)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- persistent
    user_record(name:atom, pass:atom, email:atom, role:oneof([user, administrator])).

attach_user_db(File) :-
    db_attach(File, []).

detach_user_db :- db_detach.

sync_user_db(What) :-
    db_sync(What).

current_user(Name, Pass, Email, Role) :-
    with_mutex(user_model, user_record(Name, Pass, Email, Role)).

add_user(Name, Pass, Email, Role) :-
    findall(X, current_user(X, _, _, _), Names),
    \+(member(Name, Names)),
    assert_user_record(Name, Pass, Email, Role).

update_user(Name, Pass, Email, Role) :-
    with_mutex(user_model,
                (   retractall_user_record(Name, _, _, _),
                    assert_user_record(Name, Pass, Email, Role))).

delete_user(Name) :-
    with_mutex(user_model,
                retractall_user_record(Name, _, _, _)).