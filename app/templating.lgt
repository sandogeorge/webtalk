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

:- object(templating).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/10/20,
        comment is 'Utility predicates for template handling.'
    ]).

    :- public(globals/1).
    :- dynamic(globals/1).

    :- public(init/0).
    :- info(init/0, [
        comment is 'Get global config variables to be passed to templates.'
    ]).
    init :-
        dict_create(Dict, _, []),
        ::asserta(globals(Dict)),
        user:app_config(AppConfig),
        findall(X, (
            AppConfig::current_predicate(Y), %% get config directive
            compound_name_arguments(Y, _, Args), %% extract name/arity pair
            lists:nth0(0, Args, X) %% extract name
            ), Configs),
        ::build_globals(Configs).

    :- public(render_from_base/7).
    :- info(render_from_base/7, [
        comment is 'Render the supplied template as part of the base template.',
        argnames is [
            'Template', 'Data', 'Title', 'AStyles',
            'AClasses', 'AScripts', 'Render'
        ]
    ]).
    render_from_base(Template, Data, Title, AStyles, AClasses, AScripts, Render) :-
        ::globals(GlobalData),
        lists:union([
            '/static/css/style.css'
        ], AStyles, Styles),
        atom_string(Template, TStr),
        lists:union([
            TStr
        ], AClasses, Classes),
        lists:union([
            '/static/js/script.js'
        ], AScripts, Scripts),
        put_dict(GlobalData, Data, TData),
        ::parse_template(template(Template), TData, Content),
        dict_create(BData, _, [
            title: Title,
            styles: Styles,
            body_classes: Classes,
            scripts: Scripts,
            page_content: Content
        ]),
        put_dict(GlobalData, BData, BaseData),
        ::parse_template(template(base), BaseData, Base),
        string_concat('Content-type: text/html~n~n', Base, Render).

    :- private(parse_template/3).
    :- info(parse_template/3, [
        comment is 'Parse a template file, replacing vars with supplied data.',
        argnames is ['Template', 'Data', 'Content']
    ]).
    parse_template(Template, Data, Content) :-
        memfile:new_memory_file(Handle),
        memfile:open_memory_file(Handle, write, Out),
        dict_create(StOptions, _, [frontend:semblance]),
        st_render:st_render_file(Template, Data, Out, StOptions),
        close(Out),
        memfile:open_memory_file(Handle, read, In, [free_on_close(true)]),
        read_string(In, _, Content),
        close(In).

    :- private(build_globals/1).
    :- info(build_globals/1, [
        comment is 'Populate template global dict from application config.'
    ]).
    build_globals([]).
    build_globals([Item|Predicates]) :-
        user:app_config(AppConfig),
        Callable =.. [Item, Value],
        call(AppConfig::Callable),
        dict_create(NewData, _, [Item:Value]),
        ::globals(Globals),
        ::retract(globals(_)),
        put_dict(NewData, Globals, NewGlobals),
        ::asserta(globals(NewGlobals)),
        build_globals(Predicates).

:- end_object.