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

:- protocol(formfield).
    :- public([label_markup/1, field_markup/1, help_markup/1, dict/1]).
:- end_protocol.

:- object(field,
    implements(formfield)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Base object for all fields.'
    ]).

    :- public(new/2).
    new(Instance, Clauses) :-
        self(Self),
        create_object(Instance, [extends(Self)], [], Clauses),
        Instance::init.

    :- public(spec/1).
    :- dynamic(spec/1).

    :- private(init/0).
    init :-
        ::spec(Spec),
        ((_{validators: Validators} :< Spec) ->
            ::assertz(validators(Validators))
        ; true),
        ((_{validator_hooks: Hooks} :< Spec) ->
            ::assertz(validator_hooks(Hooks))
        ; true).

    :- private(errors/1).
    :- dynamic(errors/1).
    :- info(errors/1, [
        comment is 'Field validation errors.'
    ]).
    errors([]).

    :- public(validators/1).
    :- dynamic(validators/1).
    :- info(validators/1, [
        comment is 'Validators, supplied by this library, to be used.'
    ]).
    validators([]).

    :- public(validator_hooks/1).
    :- dynamic(validator_hooks/1).
    :- info(validator_hooks/1, [
        comment is 'External goals to be called to validate field data.'
    ]).

    :- public(value/1).
    :- dynamic(value/1).
    :- info(value/1, [
        comment is 'Data value of the field.'
    ]).

    :- protected(get_attributes/2).
    get_attributes(Spec, Attributes) :-
        ((_{attributes: _} :< Spec) ->
            Attributes = Spec.attributes
        ; Attributes = _{}).

    :- protected(get_content/2).
    get_content(Spec, Content) :-
        ((_{content: _} :< Spec) ->
            Content = Spec.content
        ; Content = '').

:- end_object.

:- object(input_field,
    extends(field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Base input field.'
    ]).

    :- protected(input_type/1).

    label_markup(Markup) :-
        ::spec(Spec),
        ::get_content(Spec.label, Content),
        ::get_attributes(Spec.label, Attributes),
        form_label_widget::new(Widget, [
            content(Content),
            attributes(Attributes)
        ]),
        Widget::markup(Markup).

    field_markup(Markup) :-
        ::input_type(Type),
        ::spec(Spec),
        ::get_attributes(Spec, Attributes),
        atomic_list_concat(['form_', Type, 'input_widget'], Object),
        Object::new(Widget, [attributes(Attributes)]),
        Widget::markup(Markup).

    dict(Dict) :-
        ::label_markup(Label),
        ::field_markup(Field),
        dict_create(Dict, _, [
            label: Label,
            field: Field
        ]).

:- end_object.

:- object(button_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Button input field.'
    ]).

    input_type(button).

:- end_object.

:- object(checkbox_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Checkbox input field.'
    ]).

    input_type(checkbox).

:- end_object.

:- object(color_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Color input field.'
    ]).

    input_type(color).

:- end_object.

:- object(date_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Date input field.'
    ]).

    input_type(date).

:- end_object.

:- object(datetime_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Datetime input field.'
    ]).

    input_type('datetime-local').

:- end_object.

:- object(email_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Email input field.'
    ]).

    input_type(email).

:- end_object.

:- object(file_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'File input field.'
    ]).

    input_type(file).

:- end_object.

:- object(hidden_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Hidden input field.'
    ]).

    input_type(hidden).

:- end_object.

:- object(image_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Image input field.'
    ]).

    input_type(image).

:- end_object.

:- object(month_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Month input field.'
    ]).

    input_type(month).

:- end_object.

:- object(number_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Number input field.'
    ]).

    input_type(number).

:- end_object.

:- object(password_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Password input field.'
    ]).

    input_type(password).

:- end_object.

:- object(radio_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Radio input field.'
    ]).

    input_type(radio).

:- end_object.

:- object(range_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Range input field.'
    ]).

    input_type(range).

:- end_object.

:- object(reset_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Reset input field.'
    ]).

    input_type(reset).

:- end_object.

:- object(search_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Search input field.'
    ]).

    input_type(search).

:- end_object.

:- object(inputsubmit_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Submit input field.'
    ]).

    input_type(submit).

:- end_object.

:- object(tel_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Tel input field.'
    ]).

    input_type(tel).

:- end_object.

:- object(text_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Text input field.'
    ]).

    input_type(text).

:- end_object.

:- object(time_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Time input field.'
    ]).

    input_type(time).

:- end_object.

:- object(url_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Url input field.'
    ]).

    input_type(url).

:- end_object.

:- object(week_field,
    extends(input_field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Week input field.'
    ]).

    input_type(week).

:- end_object.

:- object(select_field,
    extends(field)).

    :- info([
        version is 1.1,
        author is 'Sando George',
        date is 2018/12/24,
        comment is 'Select field.'
    ]).

    :- private(get_options/2).
    get_options(Spec, Options) :-
        ::option_list(Spec.options, List),
        atomic_list_concat(List, '~n', Options).

    :- private(option_list/2).
    option_list([], []).
    option_list([Dict | Dicts], [Option | Options]) :-
        ::option_markup(Dict, Option),
        option_list(Dicts, Options).

    :- private(option_markup/2).
    option_markup(Dict, Markup) :-
        form_option_widget::new(Widget, [
            content(Dict.label),
            attributes(_{value: Dict.value})
        ]),
        Widget::markup(Markup).

    label_markup(Markup) :-
        ::spec(Spec),
        ::get_content(Spec.label, Content),
        ::get_attributes(Spec.label, Attributes),
        form_label_widget::new(Widget, [
            content(Content),
            attributes(Attributes)
        ]),
        Widget::markup(Markup).

    field_markup(Markup) :-
        ::spec(Spec),
        ::get_options(Spec, Content),
        ::get_attributes(Spec, Attributes),
        form_select_widget::new(Widget, [
            content(Content),
            attributes(Attributes)
        ]),
        Widget::markup(Markup).

    dict(Dict) :-
        ::label_markup(Label),
        ::field_markup(Field),
        dict_create(Dict, _, [
            label: Label,
            field: Field
        ]).

:- end_object.

:- object(buttonsubmit_field,
    extends(field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Submit field.'
    ]).

    field_markup(Markup) :-
        ::spec(Spec),
        ::get_content(Spec, Content),
        ::get_attributes(Spec, Attributes),
        form_submitbutton_widget::new(Widget, [
            content(Content),
            attributes(Attributes)
        ]),
        Widget::markup(Markup).

    dict(Dict) :-
        ::field_markup(Field),
        dict_create(Dict, _, [
            field: Field
        ]).

:- end_object.

:- object(textarea_field,
    extends(field)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/09,
        comment is 'Textarea field.'
    ]).

    label_markup(Markup) :-
        ::spec(Spec),
        ::get_content(Spec.label, Content),
        ::get_attributes(Spec.label, Attributes),
        form_label_widget::new(Widget, [
            content(Content),
            attributes(Attributes)
        ]),
        Widget::markup(Markup).

    field_markup(Markup) :-
        ::spec(Spec),
        ::get_content(Spec, Content),
        ::get_attributes(Spec, Attributes),
        form_textarea_widget::new(Widget, [
            content(Content),
            attributes(Attributes)
        ]),
        Widget::markup(Markup).

    dict(Dict) :-
        ::label_markup(Label),
        ::field_markup(Field),
        dict_create(Dict, _, [
            label: Label,
            field: Field
        ]).

:- end_object.