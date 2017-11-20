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

:- use_module(library(apply_macros)).
:- use_module(library(apply)).
:- use_module(library(broadcast)).
:- use_module(library(clpfd)).
:- use_module(library(crypto)).
:- use_module(library(http/http_session)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pcre)).
:- use_module(library(yall)).

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

%% Abstract paths.
:- multifile http:location/3.
:- dynamic http:location/3.
http:location(auth, root('auth'), []).

%% Authentication endpoints.
:- http_handler(auth('login'),
    [Request]>>(auth::login(Request)), [id("auth.login")]).
:- http_location_by_id("auth.login", Loc),
    ext_menu::add_menu_item(user, 'Log in', Loc, 99),
    ext_permission::set_path_permissions(Loc, [[auth, is_authenticated, false]]).

:- http_handler(auth('logout'),
    [Request]>>(auth::logout(Request)), [id("auth.logout")]).
:- http_location_by_id("auth.logout", Loc),
    ext_menu::add_menu_item(user, 'Log out', Loc, 99),
    ext_permission::set_path_permissions(Loc, [[auth, is_authenticated]]).

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
        argnames is ['Request']
    ]).
    login(Request) :-
        dict_create(Spec, _, [fields: _{
            login_uname_email: _{
                type: text,
                attributes: _{
                    name: login_uname_email,
                    class: ['form-control'],
                    required: []
                },
                label: _{
                    content: 'Email or username',
                    attributes: _{}
                }
            },
            login_pass: _{
                type: password,
                attributes: _{
                    name: login_pass,
                    class: ['form-control'],
                    required: []
                },
                label: _{
                    content: 'Password',
                    attributes: _{}
                }
            },
            submit: _{
                type: buttonsubmit,
                content: 'Submit',
                attributes: _{
                    name: submit,
                    class: ['btn', 'btn-primary']
                }
            }
        }]),
        form::new(Form, [spec(Spec)]),
        ((Form::validate(Request, Data), handle_login(Form, Data)) ->
            templating::flash('Login successful.', 'success'),
            lists:member(path(Base), Request),
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
            templating::render_from_base('auth/login', Data, Render),
            format(Render)
        ).
    :- private(handle_login/2).
    handle_login(Form, Data) :-
        model::new(User, [name(user)]),
        swi_option:option(login_uname_email(UoE), Data),
        swi_option:option(login_pass(Pass), Data),
        ((User::exec(current, [UoE, Hash, Email, _])
          ; User::exec(current, [Username, Hash, UoE, _])) ->
            (\+(crypto:crypto_password_hash(Pass, Hash)) ->
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
        argnames is ['Request']
    ]).
    logout(Request) :-
        ::new_session,
        templating::flash('Logout successful.', 'success'),
        lists:member(path(Base), Request),
        routing::redirect(root('.'), Base).

    :- public(is_authenticated/0).
    is_authenticated :-
        http_session:http_session_data(logged_in(_)).

    :- public(is_authenticated/1).
    is_authenticated(Out) :-
        (::is_authenticated ->
            Out = true
        ;
            Out = false).

    :- public(is_admin/0).
    is_admin :-
        ::is_authenticated,
        model::new(User, [name(user)]),
        http_session:http_session_data(user_name(Username)),
        User::exec(current, [Username, _, _, Role]),
        Role == 'administrator'.

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