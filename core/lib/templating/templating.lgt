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
:- use_module(library(http/http_session)).
:- use_module(library(lists)).
:- use_module(library(memfile)).
:- use_module(library(pcre)).
:- use_module(library(st/st_render)).
:- use_module(library(st/st_expr)).
:- use_module(library(yall)).

%% Template functions.
:- st_set_function(url_for, 1,
    [Id, Out]>>(templating::url_for(Id, Out))).

:- object(templating).

    :- info([
        version is 1:3:0,
        author is 'Sando George',
        date is 2018-12-24,
        comment is 'Utility predicates for template handling.'
    ]).

    :- private(default_data/1).
    :- dynamic(default_data/1).

    :- private(hook_data/1).
    :- dynamic(hook_data/1).

    :- private(default_styles/1).
    default_styles(['/static/css/style.css']).

    :- private(default_scripts/1).
    default_scripts(['/static/js/script.js']).

    :- public(init/0).
    :- info(init/0, [
        comment is 'Get global config variables to be passed to templates.'
    ]).
    init :-
        user:app_settings(AppSettings),
        findall(X, (AppSettings::config_property(P, V), X =.. [P, V]), Xs),
        dict_create(DefaultData, _, Xs),
        ::assertz(default_data(DefaultData)),
        dict_create(HookData, _, []),
        ::assertz(hook_data(HookData)),
        self(Self),
        ::assert_data_hook(Self, inject_flashes).

    :- private(data_hook/2).
    :- dynamic(data_hook/2).
    :- info(data_hook/2, [
        comment is 'External goals to be called to supply additional scope data.'
    ]).

    :- public(assert_data_hook/2).
    :- info(assert_data_hook/2, [
        comment is 'Register template data hooks.'
    ]).
    assert_data_hook(Object, Predicate) :-
        ((nonvar(Object), nonvar(Predicate)) ->
            ::assertz(data_hook(Object, Predicate))
        ; true).

    :- public(retract_data_hook/2).
    :- info(retract_data_hook/2, [
        comment is 'Deregister template data hooks.'
    ]).
    retract_data_hook(Object, Predicate) :-
        (nonvar(Object) ->
            ::retractall(data_hook(Object, Predicate))
        ; true).

    :- private(call_data_hooks/1).
    :- info(call_data_hooks/1, [
        comment is 'Request data from all hooks.'
    ]).
    call_data_hooks([]).
    call_data_hooks([Hook|Hooks]) :-
        Hook = [Object, Pred|_],
        Callable =.. [Pred, HookData],
        Object::Callable,
        ::hook_data(CurrentData),
        put_dict(CurrentData, HookData, UpdatedData),
        ::retractall(hook_data(_)),
        ::assertz(hook_data(UpdatedData)),
        call_data_hooks(Hooks).

    :- public(render_from_base/3).
    :- info(render_from_base/3, [
        comment is 'Render the supplied template as part of the base template.',
        argnames is ['Template', 'Data', 'Render']
    ]).
    render_from_base(Template, Data, Render) :-
        ::default_data(DefaultData),
        dict_create(Dict, _, []),
        ::retractall(hook_data(_)),
        ::assertz(hook_data(Dict)),
        findall([Obj, Pred], data_hook(Obj, Pred), Hooks),
        ::call_data_hooks(Hooks),
        ::hook_data(HookData),
        put_dict(DefaultData, HookData, HookedData),
        %
        % Merge global data with content specific data and parse contnent
        % template.
        put_dict(HookedData, Data, ContentData),
        ::find_theme_template(Template, ContentTemplate),
        ::parse_template(ContentTemplate, ContentData, Content),
        dict_create(_Content, _, [page_content:Content]),
        put_dict(_Content, ContentData, BaseDataContent),
        %
        % Merge default template styles with supplied page specific styles.
        ::default_styles(DefStyles),
        lists:union(DefStyles, BaseDataContent.styles, Styles),
        dict_create(_Styles, _, [styles:Styles]),
        put_dict(_Styles, BaseDataContent, BaseDataStyles),
        %
        % Merge default template scripts with supplied page specific scripts.
        ::default_scripts(DefScripts),
        lists:union(DefScripts, BaseDataStyles.scripts, Scripts),
        dict_create(_Scripts, _, [scripts:Scripts]),
        put_dict(_Scripts, BaseDataStyles, BaseDataAll),
        %
        % Parse base template and render final content.
        ::find_theme_template(base, BaseTemplate),
        ::parse_template(BaseTemplate, BaseDataAll, Base),
        string_concat('Content-type: text/html~n~n', Base, Render).

    :- public(render_standalone/3).
    :- info(render_standalone/3, [
        comment is 'Render the supplied template as a standalone page.',
        argnames is ['Template', 'Data', 'Render']
    ]).
    render_standalone(Template, Data, Render) :-
        ::default_data(DefaultData),
        dict_create(Dict, _, []),
        ::assertz(hook_data(Dict)),
        findall(X, (data_hook(Obj, Pred), X = [Obj, Pred]), Hooks),
        ::call_data_hooks(Hooks),
        ::hook_data(HookData),
        put_dict(DefaultData, HookData, HookedData),
        %
        % Merge global data with content specific data and parse contnent
        % template.
        put_dict(HookedData, Data, ContentData),
        %
        % Merge default template styles with supplied page specific styles.
        ::default_styles(DefStyles),
        lists:union(DefStyles, ContentData.styles, Styles),
        dict_create(_Styles, _, [styles:Styles]),
        put_dict(_Styles, ContentData, BaseDataStyles),
        %
        % Merge default template scripts with supplied page specific scripts.
        ::default_scripts(DefScripts),
        lists:union(DefScripts, BaseDataStyles.scripts, Scripts),
        dict_create(_Scripts, _, [scripts:Scripts]),
        put_dict(_Scripts, BaseDataStyles, BaseDataAll),
        %
        % Parse template and render final content.
        ::find_theme_template(Template, ThemeTemplate),
        ::parse_template(ThemeTemplate, BaseDataAll, Base),
        string_concat('Content-type: text/html~n~n', Base, Render).

    :- private(parse_template/3).
    :- info(parse_template/3, [
        comment is 'Parse a template file, replacing vars with supplied data.',
        argnames is ['Template', 'Data', 'Content']
    ]).
    parse_template(Template, Data, Content) :-
        memory_file:new_memory_file(Handle),
        memory_file:open_memory_file(Handle, write, Out),
        dict_create(StOptions, _, [frontend:semblance, undefined: false]),
        st_render:st_render_file(Template, Data, Out, StOptions),
        close(Out),
        memory_file:open_memory_file(Handle, read, In, [free_on_close(true)]),
        read_string(In, _, Content),
        close(In).

    :- private(find_theme_template/2).
    find_theme_template(Search, Template) :-
        model::new(ThemeDb, [name(theme)]),
        ThemeDb::exec(current, [Default, true]),
        atomic_list_concat([Default, '/template/', Search], ThemeSearch),
        findall(X,
            (
                absolute_file_name(theme(ThemeSearch), X, [
                    extensions([html]),
                    solutions(all)
                ]),
                exists_file(X)
            ),
        Xs),
        (Xs \== [] ->
            lists:nth0(0, Xs, First),
            pcre:re_replace("[.]html$", "", First, Template)
        ;
            throw(error(template_not_found(ThemeSearch)))).

    :- public(flash/2).
    :- info(flash/2, [
        comment is 'Register messages to she shown to the user.'
    ]).
    flash(Message, Category) :-
        dict_create(Flash, _, [message: Message, category: Category]),
        (http_session:http_session_data(flashes(Flashes)) ->
            lists:append(Flashes, [Flash], Updated),
            http_session:http_session_assert(flashes(Updated))
        ;
            http_session:http_session_assert(flashes([Flash]))).

    :- public(inject_flashes/1).
    :- info(inject_flashes/1, [
        comment is 'Make flash messages available to templates.'
    ]).
    inject_flashes(Flashes) :-
        var(Flashes),
        (http_session:http_session_data(flashes(F)) ->
            dict_create(Flashes, _, [messages: F]),
            http_session:http_session_retract(flashes(_))
        ; dict_create(Flashes, _, [messages: []])).

    :- public(url_for/2).
    :- info(url_for/2, [
        comment is 'Given a handler ID, return the url.'
    ]).
    url_for(Id, Out) :-
        atom_string(IdAtom, Id),
        user:http_location_by_id(IdAtom, Out).

:- end_object.
