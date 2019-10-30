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
:- use_module(library(crypto)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pcre)).
:- use_module(library(yall)).

% Installation endpoints.
:- http_handler(root('install'),
    [Request]>>(install::install(Request)), [id('install')]).

:- object(install).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/04,
        comment is 'Defines handlers installation pages.'
    ]).

    :- public(install/1).
    :- info(install/1, [
        comment is 'Present for initial configuration'
    ]).
    install(Request) :-
        dict_create(Spec, _, [
            fields: _{
                admin_username: _{
                    type: text,
                    attributes: _{
                        name: admin_username,
                        class: ['form-control'],
                        placeholder: 'superbad username',
                        required: []
                    },
                    label: _{
                        content: 'Username',
                        attributes: _{}
                    }
                },
                admin_password: _{
                    type: password,
                    attributes: _{
                        name: admin_password,
                        class: ['form-control'],
                        placeholder: 'strong password',
                        required: []
                    },
                    label: _{
                        content: 'Password',
                        attributes: _{}
                    }
                },
                admin_email: _{
                    type: email,
                    attributes: _{
                        name: admin_email,
                        class: ['form-control'],
                        placeholder: 'working email',
                        required: []
                    },
                    label: _{
                        content: 'Email address',
                        attributes: _{}
                    }
                },
                submit: _{
                    type: buttonsubmit,
                    content: 'Install',
                    attributes: _{
                        name: submit,
                        class: ['btn', 'btn-primary']
                    }
                }
            }
        ]),
        form::new(Form, [spec(Spec)]),
        ((Form::validate(Request, Data), handle_install_post(Data)) ->
            lists:member(path(Base), Request),
            routing::redirect(root('.'), Base)
        ;
            Form::dict(FormDict),
            dict_create(Data, _, [
                title: 'Install Webtalk',
                page_header: 'Install Webtalk',
                styles: [],
                scripts: ['/static/js/install.js'],
                body_classes: ['install'],
                form: FormDict
            ]),
            templating::render_standalone('install/index', Data, Render),
            format(Render)
        ).

    :- private(handle_install_post/1).
    handle_install_post(Data) :-
        swi_option:option(admin_username(Username), Data),
        swi_option:option(admin_email(Email), Data),
        swi_option:option(admin_password(Password), Data),
        pcre:re_match("^.+[@].+[.].{2,}$", Email),
        crypto:crypto_password_hash(Password, Hash),
        model::new(User, [name(user)]),
        \+(User::exec(current, [Username, _, _, _])),
        \+(User::exec(current, [_, _, Email, _])),
        User::exec(add, [Username, Hash, Email, administrator]),
        model::new(Flag, [name(flag)]),
        Flag::exec(add, [installed, true]).

:- end_object.
