% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This file is part of Webtalk <https://github.com/sandogeorge/webtalk>.
% Copyright (c) 2017 Sando George <sando.csc AT gmail.com>
%
% Webtalk is free software: you can redistribute it and/or modify it under
% the terms of the GNU General Public License as published by the Free Software
% Foundation, either version 3 of the License, or (at your option) any later
% version.
%
% Webtalk is distributed in the hope that it will be useful, but WITHOUT ANY
% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
% A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License along with
% Webtalk. If not, see <http://www.gnu.org/licenses/>.
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(crypto)).
:- st_expr:st_set_function(is_authenticated, 0,
                            [Out]>>(auth::is_authenticated(Out))).

:- object(auth).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/03,
        comment is 'Defines handlers for authentication pages.'
    ]).

    :- initialization(init).
    :- private(init/0).
    init :-
        routing::assert_expansion_hook(auth, validate_login_access),
        routing::assert_expansion_hook(auth, validate_logout_access).

    :- public(login/1).
    :- info(login/1, [
        comment is 'Serve login page',
        argnames is ['_Request']
    ]).
    login(_Request) :-
        lists:member(method(Method), _Request),
        ((Method == 'post', handle_login(_Request)) ->
            lists:member(path(Base), _Request),
            routing::redirect(root('.'), Base)
        ;
            dict_create(Data, _, [
                title: 'Login',
                page_header: 'Login',
                styles: [],
                scripts: [],
                body_classes: ['auth login']
            ]),
            templating::render_from_base(login, Data, Render),
            format(Render)
        ).
    :- private(handle_login/1).
    handle_login(_Request) :-
        http_parameters:http_parameters(_Request, [
            login_uname_email(UoE, [atom]),
            login_pass(Pass, [atom])
        ]),
        user:get_model(user, User),
        ((User::exec(current, [UoE, Hash, _, _])
          ; User::exec(current, [_, Hash, UoE, _])) ->
            crypto:crypto_password_hash(Pass, Hash),
            http_session:http_session_assert(logged_in(true))
        ; false).

    :- public(logout/1).
    :- info(logout/1, [
        comment is 'Log out authenticated user.',
        argnames is ['_Request']
    ]).
    logout(_Request) :-
        ((::is_authenticated(Auth), Auth, http_session:http_session_id(Id)) ->
            http_session:http_close_session(Id)
        ; true),
        lists:member(path(Base), _Request),
        routing::redirect(root('.'), Base).

    :- public(is_authenticated/1).
    is_authenticated(Out) :-
        (http_session:http_session_data(logged_in(_)) ->
            Out = true
        ;
            Out = false).

    :- public(validate_login_access/1).
    :- info(validate_login_access/1, [
        comment is 'Redirect to index if the user is already logged in.'
    ]).
    validate_login_access(_Request) :-
        lists:member(path(Path), _Request),
        ::is_authenticated(Bool),
        ((Bool, pcre:re_match("^/auth/login[/]?", Path)) ->
            routing::redirect(root('.'), Path)
        ; true).

    :- public(validate_logout_access/1).
    :- info(validate_logout_access/1, [
        comment is 'Redirect to index if the user is not logged in.'
    ]).
    validate_logout_access(_Request) :-
        lists:member(path(Path), _Request),
        ::is_authenticated(Bool),
        ((not(Bool), pcre:re_match("^/auth/logout[/]?", Path)) ->
            routing::redirect(root('.'), Path)
        ; true).

:- end_object.