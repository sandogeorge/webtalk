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
:- use_module(library(broadcast)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).

%% Session configuration.
% Set general sessions options.
:- http_session:http_set_session_options([
    cookie('session_id'),
    route(''),
    gc(active)
]).
% Generate a CSRF token for each session.
:- listen(http_session(begin(SessionId, _)),
            auth::generate_csrf_token(SessionId)).

:- object(auth).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/03,
        comment is 'Defines handlers for authentication pages.'
    ]).

    :- use_module(clpfd, [
        in/2, (#=)/2, (#\=)/2,
        op(700, xfx, #=)
    ]).

    :- initialization(init).
    :- private(init/0).
    init :-
        routing::assert_expansion_hook(auth, validate_login_access),
        routing::assert_expansion_hook(auth, validate_logout_access),
        templating::assert_data_hook(auth, inject_current_user).

    :- private(new_session/0).
    :- info(new_session/0, [
        comment is 'Close current session and start a new one.'
    ]).
    new_session :-
        http_session:http_session_id(Id),
        http_session:http_close_session(Id),
        http_session:http_open_session(_, [renew(true)]).

    :- public(login/1).
    :- info(login/1, [
        comment is 'Serve login page',
        argnames is ['_Request']
    ]).
    login(_Request) :-
        dict_create(Spec, _, [fields: [
            login_uname_email: [
                type: text,
                label: 'Email or username'
            ],
            login_pass: [
                type: password,
                label: 'Password'
            ],
            submit: [
                type: submit
            ]
        ]]),
        form::new(Form, [spec(Spec)]),
        ((Form::validate(_Request, Data), handle_login(Form, Data)) ->
            templating::flash('Login successful.', 'success'),
            lists:member(path(Base), _Request),
            routing::redirect(root('.'), Base)
        ;
            Form::dict(FormDict),
            dict_create(Data, _, [
                title: 'Login',
                page_header: 'Login',
                styles: [],
                scripts: [],
                body_classes: ['auth login'],
                form: FormDict
            ]),
            templating::render_from_base(login, Data, Render),
            format(Render)
        ).
    :- private(handle_login/2).
    handle_login(Form, Data) :-
        model::new(User, [name(user)]),
        swi_option:option(login_uname_email(UoE), Data),
        swi_option:option(login_pass(Pass), Data),
        ((User::exec(current, [UoE, Hash, Email, _])
          ; User::exec(current, [Username, Hash, UoE, _])) ->
            (not(crypto:crypto_password_hash(Pass, Hash)) ->
                Form::assert_error('Invalid email/username and password combination.'),
                false
            ; true),
            ::new_session,
            http_session:http_session_assert(logged_in(true)),
            (var(Email) ->
                http_session:http_session_assert(user_name(Username))
            ;
                http_session:http_session_assert(user_name(UoE)))
        ;
            Form::assert_error('Invalid email/username and password combination.'),
            false).

    :- public(logout/1).
    :- info(logout/1, [
        comment is 'Log out authenticated user.',
        argnames is ['_Request']
    ]).
    logout(_Request) :-
        ::new_session,
        templating::flash('Logout successful.', 'success'),
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
            templating::flash('User already logged in.', 'warning'),
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
            templating::flash('User not logged in.', 'warning'),
            routing::redirect(root('.'), Path)
        ; true).

    :- public(inject_current_user/1).
    :- info(inject_current_user/1, [
        comment is 'Get current user dict and make it available to templates.'
    ]).
    inject_current_user(CurrentUser) :-
        var(CurrentUser),
        ::current_user(CU),
        dict_create(CurrentUser, _, [current_user: CU]).

    :- private(current_user/1).
    :- info(current_user/1, [
        comment is 'Build current user dict.'
    ]).
    current_user(CurrentUser) :-
        ((::is_authenticated(Bool), Bool) ->
            model::new(User, [name(user)]),
            http_session:http_session_data(user_name(Username)),
            User::exec(current, [Username, _, Email, Role]),
            dict_create(CurrentUser, _, [
                is_authenticated: true,
                name: Username,
                email: Email,
                role: Role
            ])
        ;
            dict_create(CurrentUser, _, [
                is_authenticated: false,
                name: anonymous
            ])).

    :- public(generate_csrf_token/1).
    :- info(generate_csrf_token/1, [
        comment is 'Generate a new CSRF synchronizer token and add to session data.'
    ]).
    generate_csrf_token(SessionId) :-
        (http_session:http_session_id(SessionId) ->
            crypto:crypto_n_random_bytes(32, Bs),
            ::bytes_integer(Bs, I),
            http_session:http_session_assert(csrf(I))
        ; true).

    % Generate 256-bit integer from a list of random bytes.
    % Source: http://www.swi-prolog.org/pldoc/man?section=crypto
    :- private(bytes_integer/2).
    bytes_integer(Bs, N) :-
        apply:foldl(::pow, Bs, 0-0, N-_).
    :- private(pow/3).
    pow(B, N0-I0, N-I) :-
        B in 0..255,
        N #= N0 + B*256^I0,
        I #= I0 + 1.

:- end_object.