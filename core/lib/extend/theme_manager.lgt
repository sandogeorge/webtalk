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

:- use_module(library(http/json)).
:- use_module(library(lists)).

:- object(theme_manager).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017-11-20,
        comment is 'Predicates used for managing themes.'
    ]).

    :- public(install_default_themes/0).
    install_default_themes :-
        (::install_theme(base) -> true ; true),
        ::set_default(base).

    :- public(install_theme/1).
    :- info(install_theme/1, [
        comment is 'Install a theme that exists in the search path.'
    ]).
    install_theme(Theme) :-
        model::new(ThemeDb, [name(theme)]),
        atom_concat('theme_', Theme, Object),
        atomic_list_concat([Theme, '/', Object], Search),
        ::find_theme(Search, Path),
        logtalk_load(Path),
        Object::install,
        \+(ThemeDb::exec(current, [Theme, _])),
        ThemeDb::exec(add, [Theme, false]).

    :- public(uninstall_theme/1).
    :- info(uninstall_theme/1, [
        comment is 'Uninstall a theme that exists in the search path.'
    ]).
    uninstall_theme(Theme) :-
        model::new(ThemeDb, [name(theme)]),
        atom_concat('theme_', Theme, Object),
        Object::uninstall,
        ThemeDb::exec(current, [Theme, _]),
        ThemeDb::exec(delete, [Theme]).

    :- public(set_default/1).
    :- info(set_default/1, [
        comment is 'Make the specified theme the default theme.'
    ]).
    set_default(Theme) :-
        model::new(ThemeDb, [name(theme)]),
        ThemeDb::exec(current, [Theme, _]),
        (\+ ThemeDb::exec(current, [Theme, true]) ->
            (ThemeDb::exec(current, [Default, true]) ->
                ThemeDb::exec(update, [Default, false])
            ; true),
            ThemeDb::exec(update, [Theme, true])
        ; true).

    :- private(find_theme/2).
    :- info(find_theme/2, [
        comment is 'Get absolute path to theme file.'
    ]).
    find_theme(Name, Path) :-
        findall(X,
            (
                absolute_file_name(theme(Name), X, [
                    extensions([lgt]),
                    solutions(all)
                ]),
                exists_file(X)
            ),
        Xs),
        (Xs \== [] ->
            lists:nth0(0, Xs, Path)
        ;
            throw(error(theme_not_found(Name)))).

    :- public(get_themes/1).
    :- info(get_themes/1, [
        comment is 'Get list of theme info dicts.'
    ]).
    get_themes(Themes) :-
        findall(P, (expand_file_search_path(theme(.), P)), Paths),
        meta::map(directory_files, Paths, Files),
        lists:flatten(Files, FlatFiles),
        meta::map([X, Y]>>(atom_concat(X, '/theme', Y)), FlatFiles, Candidates),
        meta::map({A, ThemeFile, Stream}/[X, Y]>>(findall(A,
            (
                absolute_file_name(theme(X), ThemeFile, [
                    extensions([json]),
                    solutions(all)
                ]),
                exists_file(ThemeFile),
                open(ThemeFile, read, Stream),
                json:json_read_dict(Stream, Dict, [value_string_as(atom)]),
                select_dict(_{name: A}, Dict, _)
            ), Y)
        ), Candidates, Found),
        lists:flatten(Found, Themes).

    :- public(get_themes_dict/1).
    :- info(get_themes_dict/1, [
        comment is 'Get list of theme info dicts.'
    ]).
    get_themes_dict(Themes) :-
        model::new(ThemeDb, [name(theme)]),
        findall(P, (expand_file_search_path(theme(.), P)), Paths),
        meta::map(directory_files, Paths, Files),
        lists:flatten(Files, FlatFiles),
        meta::map([X, Y]>>(atom_concat(X, '/theme', Y)), FlatFiles, Candidates),
        meta::map({A, ThemeFile, Stream, ThemeDb, IsDefault}/[X, Y]>>(findall(A,
            (
                absolute_file_name(theme(X), ThemeFile, [
                    extensions([json]),
                    solutions(all)
                ]),
                exists_file(ThemeFile),
                open(ThemeFile, read, Stream),
                json:json_read_dict(Stream, Dict, [value_string_as(atom)]),
                select_dict(_{name: Name}, Dict, _),
                (ThemeDb::exec(current, [Name, IsDefault]) ->
                    put_dict(Dict, newdict{installed: true, default: IsDefault}, A)
                ;
                    put_dict(Dict, newdict{installed: false, default: false}, A))
            ), Y)
        ), Candidates, Found),
        lists:flatten(Found, Themes).

:- end_object.