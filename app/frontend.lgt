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

:- object(frontend).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/10/19,
        comment is 'Defines handlers for HTML pages.'
    ]).

    :- public(index/1).
    :- info(index/1, [
        comment is 'Index page (/).',
        argnames is ['_Request']
    ]).
    index(_Request) :-
        dict_create(Data, _, []),
        render_from_base(index, Data, 'Home', [], [], [], Render),
        format(Render).

    :- public(static/1).
    :- info(static/1, [
        comment is 'Serve files from the static directory.',
        argnames is ['_Request']
    ]).
    static(_Request) :-
        lists:member(path(Path), _Request),
        expand_file_search_path(app(Path), Expanded),
        exists_file(Expanded),
        http_files:http_reply_from_files(app('./static'), [], _Request).
    static(_Request) :-
        http_dispatch:http_404([], _Request).

    :- private(render_from_base/7).
    :- info(render_from_base/7, [
        comment is 'Render the supplied template as part of the base template.',
        argnames is [
            'Template', 'Data', 'Title', 'AStyles',
            'AClasses', 'AScripts', 'Render'
        ]
    ]).
    render_from_base(Template, Data, Title, AStyles, AClasses, AScripts, Render) :-
        user:app_config(AppConfig),
        call(AppConfig::site_name(Sitename)),
        call(AppConfig::jquery_version(JQueryV)),
        call(AppConfig::popper_version(PopperV)),
        call(AppConfig::bootstrap_version(BStrapV)),
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
        ::parse_template(template(Template), Data, Content),
        dict_create(BaseData, _, [
            title: Title,
            styles: Styles,
            body_classes: Classes,
            scripts: Scripts,
            page_content: Content,
            site_name: Sitename,
            jquery_version: JQueryV,
            popper_version: PopperV,
            bootstrap_version: BStrapV
        ]),
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

:- end_object.