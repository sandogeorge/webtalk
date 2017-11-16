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

:- object(extension_manager).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/15,
        comment is 'Predicates used for managing extensions.'
    ]).

    :- private(default_extensions/1).
    default_extensions(['permission', 'menu']).

    :- public(install_default_extensions/0).
    install_default_extensions :-
        ::default_extensions(Exts),
        ::install_extensions(Exts).

    :- private(install_extensions/1).
    :- info(install_extensions/1, [
        comment is 'Iterate over a list of extensions and install each.'
    ]).
    install_extensions([]).
    install_extensions([Ext | Exts]) :-
        ::install_extension(Ext),
        ::install_extensions(Exts).

    :- private(install_extension/1).
    :- info(install_extension/1, [
        comment is 'Install an extension that exists in the search path.'
    ]).
    install_extension(Ext) :-
        atom_concat('ext_', Ext, Object),
        atomic_list_concat([Ext, '/', Object], Search),
        ::find_extension(Search, Path),
        logtalk_load(Path),
        Object::install.

    :- private(find_extension/2).
    :- info(find_extension/2, [
        comment is 'Get absolute path to extension file.'
    ]).
    find_extension(Name, Path) :-
        findall(X,
            (
                absolute_file_name(extension(Name), X, [
                    extensions([lgt]),
                    solutions(all)
                ]),
                exists_file(X)
            ),
        Xs),
        (Xs \== [] ->
            lists:nth0(0, Xs, Path)
        ;
            throw(error(extension_not_found(Name)))).

:- end_object.