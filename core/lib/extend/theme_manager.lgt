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

:- use_module(library(lists)).

:- object(theme_manager).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/20,
        comment is 'Predicates used for managing themes.'
    ]).

    :- public(install_default_themes/0).
    install_default_themes :-
        user:app_settings(AppSettings),
        AppSettings::config_property(theme, Theme),
        ::install_theme(Theme).

    :- private(install_theme/1).
    :- info(install_theme/1, [
        comment is 'Install a theme that exists in the search path.'
    ]).
    install_theme(Theme) :-
        atom_concat('theme_', Theme, Object),
        atomic_list_concat([Theme, '/', Object], Search),
        ::find_theme(Search, Path),
        logtalk_load(Path),
        Object::install.

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

:- end_object.