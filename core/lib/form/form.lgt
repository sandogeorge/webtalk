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

:- use_module(library(base64)).
:- use_module(library(crypto)).
:- use_module(library(dicts)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

:- object(form).

    :- info([
        version is 1.1,
        author is 'Sando George',
        date is 2017/12/24,
        comment is 'Defines predicates for working with HTML forms.'
    ]).

    :- public(new/2).
    new(Instance, Clauses) :-
        self(Self),
        create_object(Instance, [extends(Self)], [], Clauses),
        Instance::init.

    :- private(init/0).
    init :-
        (http_session:http_session_data(csrf(CSRF)) ->
            number_string(CSRF, CSRFString),
            crypto:crypto_data_hash(CSRFString, Token, [algorithm(sha512)]),
            base64:base64(Token, B64),
            ::retractall(csrf_token(_)),
            ::assertz(csrf_token(B64))
        ; true),
        ::spec(Spec),
        dicts:dict_keys(Spec.fields, Keys),
        ::create_field(Keys, Spec.fields).

    :- private(spec/1).
    :- dynamic(spec/1).

    :- private(field/2).
    :- dynamic(field/2).
    :- info(field/2, [
        comment is 'Get and set form fields.'
    ]).

    :- private(create_field/2).
    create_field([], _).
    create_field([Field | Fields], Spec) :-
        atom_concat(Spec.Field.type, '_field', Object),
        ::ensure_field_id(Spec.Field, SpecId),
        ::ensure_label_for(SpecId, SpecLabel),
        Object::new(Instance, [spec(SpecLabel)]),
        ::retractall(field(Field, _)),
        ::assertz(field(Field, Instance)),
        create_field(Fields, Spec).

    :- private(ensure_field_id/2).
    ensure_field_id(SpecIn, SpecOut) :-
        ((_{id: _} :< SpecIn.attributes) ->
            SpecOut = SpecIn
        ;
            pcre:re_replace("_"/g, "-", SpecIn.attributes.name, Id),
            atom_string(AtomId, Id),
            put_dict(_{id: AtomId}, SpecIn.attributes, Attributes),
            put_dict(_{attributes: Attributes}, SpecIn, SpecOut)).

    :- private(ensure_label_for/2).
    ensure_label_for(SpecIn, SpecOut) :-
        ((_{label: _} :< SpecIn) ->
            ((_{attributes: _} :< SpecIn.label) ->
                ((_{for: _} :< SpecIn.label.attributes) ->
                    SpecOut = SpecIn
                ;
                    put_dict(
                        _{for: SpecIn.attributes.id},
                        SpecIn.label.attributes,
                        Attributes
                    ),
                    put_dict(
                        _{attributes: Attributes},
                        SpecIn.label,
                        Label
                    ),
                    put_dict(_{label: Label}, SpecIn, SpecOut))
            ; SpecOut = SpecIn)
       ; SpecOut = SpecIn).

    :- private(csrf_token/1).
    :- dynamic(csrf_token/1).

    :- private(errors/1).
    :- dynamic(errors/1).
    errors([]).

    :- public(assert_error/1).
    :- info(assert_error/1, [
        comment is 'Add the supplied error to the list to be rendered to the user'
    ]).
    assert_error(Message) :-
        ::errors(Errors),
        lists:append(Errors, [Message], NewErrors),
        ::retractall(errors(_)),
        ::assertz(errors(NewErrors)).

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
        (\+(::validate_csrf_token(CSRFToken)) ->
            ::assert_error('Expired session. Please try again.'),
            false
        ; true).

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
        ::errors(Errors),
        dict_create(FormDict, _, [
            csrf_token: FormCSRF,
            errors: Errors
        ]),
        findall(X, ::field(X, _), Xs),
        ::field_dicts(Xs, Fields),
        dict_create(FieldDicts, _, Fields),
        put_dict(FormDict, FieldDicts, Dict),
        ::retractall(errors(_)),
        ::assertz(errors([])).

    :- private(field_dicts/2).
    field_dicts([], []).
    field_dicts([Field | Fields], [Dict | Dicts]) :-
        ::field(Field, Instance),
        Instance::dict(FieldDict),
        Dict = Field:FieldDict,
        field_dicts(Fields, Dicts).

:- end_object.