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

:- use_module(library(dicts)).

:- protocol(html_markup).

    :- public([markup/1]).

:- end_protocol.

:- object(html_widget,
    implements(html_markup)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'Base HTML widget.'
    ]).

    :- public(new/2).
    new(Instance, Clauses) :-
        self(Self),
        create_object(Instance, [extends(Self)], [], Clauses).

    :- public(content/1).
    :- dynamic(content/1).

    :- public(attributes/1).
    :- dynamic(attributes/1).

    :- private(attribute_list/2).
    attribute_list([], []).
    attribute_list([Attr | Attrs], [String | Strings]) :-
        ::attributes(Attributes),
        (is_list(Attributes.Attr) ->
            atomic_list_concat(Attributes.Attr, ' ', Properties)
        ;
            Properties = Attributes.Attr
        ),
        format(atom(String), '~s="~s"', [Attr, Properties]),
        attribute_list(Attrs, Strings).

    :- protected(attribute_string/1).
    attribute_string(String) :-
        ::attributes(Attributes),
        dicts:dict_keys(Attributes, Keys),
        ::attribute_list(Keys, List),
        atomic_list_concat(List, ' ', String).

:- end_object.

:- object(form_label_widget,
    extends(html_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <label> widget.'
    ]).

    markup(Markup) :-
        ::content(Content),
        ::attribute_string(Attributes),
        format(atom(Markup), '<label ~s>~s</label>', [Attributes, Content]).

:- end_object.

:- object(form_input_widget,
    extends(html_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input> widget.'
    ]).

    :- protected(input_type/1).

    markup(Markup) :-
        ::input_type(Type),
        ::attribute_string(Attributes),
        format(atom(Markup), '<input type="~s" ~s>', [Type, Attributes]).

:- end_object.

:- object(form_buttoninput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="button"> widget.'
    ]).

    input_type(button).

:- end_object.

:- object(form_checkboxinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="checkbox"> widget.'
    ]).

    input_type(checkbox).

:- end_object.

:- object(form_colorinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="color"> widget.'
    ]).

    input_type(color).

:- end_object.

:- object(form_dateinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="date"> widget.'
    ]).

    input_type(date).

:- end_object.

:- object(form_datetimeinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="date"> widget.'
    ]).

    input_type('datetime-local').

:- end_object.

:- object(form_emailinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="email"> widget.'
    ]).

    input_type(email).

:- end_object.

:- object(form_fileinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="file"> widget.'
    ]).

    input_type(file).

:- end_object.

:- object(form_hiddeninput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="hidden"> widget.'
    ]).

    input_type(hidden).

:- end_object.

:- object(form_imageinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="image"> widget.'
    ]).

    input_type(image).

:- end_object.

:- object(form_monthinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="month"> widget.'
    ]).

    input_type(month).

:- end_object.

:- object(form_numberinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="number"> widget.'
    ]).

    input_type(number).

:- end_object.

:- object(form_passwordinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="password"> widget.'
    ]).

    input_type(password).

:- end_object.

:- object(form_radioinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="radio"> widget.'
    ]).

    input_type(radio).

:- end_object.

:- object(form_rangeinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="range"> widget.'
    ]).

    input_type(range).

:- end_object.

:- object(form_resetinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="reset"> widget.'
    ]).

    input_type(reset).

:- end_object.

:- object(form_searchinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="search"> widget.'
    ]).

    input_type(search).

:- end_object.

:- object(form_submitinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="submit"> widget.'
    ]).

    input_type(submit).

:- end_object.

:- object(form_telinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="tel"> widget.'
    ]).

    input_type(tel).

:- end_object.

:- object(form_textinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="text"> widget.'
    ]).

    input_type(text).

:- end_object.

:- object(form_timeinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="time"> widget.'
    ]).

    input_type(time).

:- end_object.

:- object(form_urlinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="url"> widget.'
    ]).

    input_type(url).

:- end_object.

:- object(form_weekinput_widget,
    extends(form_input_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <input type="week"> widget.'
    ]).

    input_type(week).

:- end_object.

:- object(form_button_widget,
    extends(html_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <button> widget.'
    ]).

    :- protected(input_type/1).

    markup(Markup) :-
        ::input_type(Type),
        ::attribute_string(Attributes),
        ::content(Content),
        format(atom(Markup), '<button type="~s" ~s>~s</button>', [
            Type,
            Attributes,
            Content
        ]).

:- end_object.

:- object(form_submitbutton_widget,
    extends(form_button_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <button type="submit"> widget.'
    ]).

    input_type(submit).

:- end_object.

:- object(form_option_widget,
    extends(html_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <option> widget.'
    ]).

    markup(Markup) :-
        ::attribute_string(Attributes),
        ::content(Content),
        format(atom(Markup), '<option ~s>~s</option>', [
            Attributes,
            Content
        ]).

:- end_object.

:- object(form_select_widget,
    extends(html_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <select> widget.'
    ]).

    markup(Markup) :-
        ::content(Content),
        ::attribute_string(Attributes),
        format(atom(Markup), '<select ~s>~s</select>', [
            Attributes,
            Content
        ]).

:- end_object.

:- object(form_textarea_widget,
    extends(html_widget)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/11,
        comment is 'HTML <textarea> widget.'
    ]).

    markup(Markup) :-
        ::attribute_string(Attributes),
        ::content(Content),
        format(atom(Markup), '<textarea ~s>~s</textarea>', [
            Attributes,
            Content
        ]).

:- end_object.