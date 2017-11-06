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
:- use_module(library(base64)).

% get_form(-Form)
%
% Return an object to interface the specified persistency model.
get_form(Form) :-
    create_object(Form,
        [extends(form)],
        [initialization(::init)],
        []
    ).

:- object(form).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/05,
        comment is 'Defines methods for working with HTML forms.'
    ]).

    :- public(init/0).
    init :-
        (http_session:http_session_data(csrf(CSRF)) ->
            number_string(CSRF, CSRFString),
            crypto:crypto_data_hash(CSRFString, Token, [algorithm(sha512)]),
            base64:base64(Token, B64),
            ::asserta(csrf_token(B64))
        ; true).

    :- private(csrf_token/1).
    :- dynamic(csrf_token/1).

    :- public(validate/2).
    :- info(validate/2, [
        comment is 'Validate the posted form data, includin the CSRF token'
    ]).
    :- meta_predicate(http_parameters:http_parameters(*, *, *)).
    validate(Request, Data) :-
        memberchk(method(post), Request),
        http_parameters:http_parameters(Request, [
            csrf_token(CSRFToken, [atom])
        ], [form_data(Data)]),
        ::validate_csrf_token(CSRFToken).

    :- private(validate_csrf_token/1).
    :- info(validate_csrf_token/1, [
        comment is 'Verify that the posted CSRF token matches that of the session'
    ]).
    validate_csrf_token(B64) :-
        base64:base64(Token, B64),
        http_session:http_session_data(csrf(CSRF)),
        number_string(CSRF, CSRFString),
        crypto:crypto_data_hash(CSRFString, Hash, [algorithm(sha512)]),
        Token == Hash.

    :- public(dict/1).
    :- info(dict/1, [
        comment is 'Return dict representation of form'
    ]).
    dict(Dict) :-
        ::csrf_token(CSRF),
        atomic_list_concat([
            '<input type="hidden" name="csrf_token" value="', CSRF, '">'
        ], FormCSRF),
        dict_create(Dict, _, [
            csrf_token: FormCSRF
        ]).

:- end_object.