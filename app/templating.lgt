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
        version is 1.1,
        author is 'Sando George',
        date is 2017/10/23,
        comment is 'Utility predicates for template handling.'
    ]).

    :- private(default_data/1).
    :- dynamic(default_data/1).

    :- private(default_styles/1).
    default_styles(['/static/css/style.css']).

    :- private(default_scripts/1).
    default_scripts(['/static/js/script.js']).

    :- public(init/0).
    :- info(init/0, [
        comment is 'Get global config variables to be passed to templates.'
    ]).
    init :-
        user:app_config(AppConfig),
        findall(X, (AppConfig::config_property(P, V), X =.. [P, V]), Xs),
        dict_create(DefaultData, _, Xs),
        ::asserta(default_data(DefaultData)).

    :- public(render_from_base/3).
    :- info(render_from_base/3, [
        comment is 'Render the supplied template as part of the base template.',
        argnames is ['Template', 'Data', 'Render']
    ]).
    render_from_base(Template, Data, Render) :-
        ::default_data(DefaultData),
        %
        % Merge global data with content specific data and parse contnent
        % template.
        put_dict(DefaultData, Data, ContentData),
        ::parse_template(template(Template), ContentData, Content),
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
        ::parse_template(template(base), BaseDataAll, Base),
        string_concat('Content-type: text/html~n~n', Base, Render).

    :- public(render_standalone/3).
    :- info(render_standlaone/3, [
        comment is 'Render the supplied template as a standalone page.',
        argnames is ['Template', 'Data', 'Render']
    ]).
    render_standalone(Template, Data, Render) :-
        ::default_data(DefaultData),
        %
        % Merge global data with content specific data and parse contnent
        % template.
        put_dict(DefaultData, Data, ContentData),
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
        % Parse base template and render final content.
        ::parse_template(template(Template), BaseDataAll, Base),
        string_concat('Content-type: text/html~n~n', Base, Render).

    :- private(parse_template/3).
    :- info(parse_template/3, [
        comment is 'Parse a template file, replacing vars with supplied data.',
        argnames is ['Template', 'Data', 'Content']
    ]).
    parse_template(Template, Data, Content) :-
        memfile:new_memory_file(Handle),
        memfile:open_memory_file(Handle, write, Out),
        dict_create(StOptions, _, [frontend:semblance, undefined: false]),
        st_render:st_render_file(Template, Data, Out, StOptions),
        close(Out),
        memfile:open_memory_file(Handle, read, In, [free_on_close(true)]),
        read_string(In, _, Content),
        close(In).

:- end_object.