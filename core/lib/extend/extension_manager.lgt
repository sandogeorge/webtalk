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
:- use_module(library(pcre)).

:- object(extension_manager).

    :- info([
        version is 1.1,
        author is 'Sando George',
        date is 2018-12-24,
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
        (::install_extension(Ext) -> true ; true),
        install_extensions(Exts).

    :- public(install_extension/1).
    :- info(install_extension/1, [
        comment is 'Install an extension that exists in the search path.'
    ]).
    install_extension(Ext) :-
        model::new(ExtDb, [name(extension)]),
        atom_concat('ext_', Ext, Object),
        atomic_list_concat([Ext, '/', Object], Search),
        ::find_extension(Search, Path),
        logtalk_load(Path),
        Object::install,
        \+(ExtDb::exec(current, [Ext, _])),
        ::get_extension_info(Ext, Info),
        (_{required: Required} :< Info ->
            ExtDb::exec(add, [Ext, Required])
        ;
            ExtDb::exec(add, [Ext, false])
        ).

    :- public(uninstall_extension/1).
    :- info(uninstall_extension/1, [
        comment is 'Uninstall an extension that exists in the search path.'
    ]).
    uninstall_extension(Ext) :-
        model::new(ExtDb, [name(extension)]),
        atom_concat('ext_', Ext, Object),
        Object::uninstall,
        ExtDb::exec(current, [Ext, _]),
        ExtDb::exec(delete, [Ext]).

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

    :- private(get_extension_info/2).
    :- info(get_extension_info/2, [
        comment is 'Get data from the json file of an extension.'
    ]).
    get_extension_info(Name, Info) :-
        findall(X,
            (
                atom_concat(Name, '/extension', Search),
                absolute_file_name(extension(Search), X, [
                    extensions([json]),
                    solutions(all)
                ]),
                exists_file(X)
            ),
        Xs),
        (Xs \== [] ->
            lists:nth0(0, Xs, ExtFile),
            open(ExtFile, read, Stream),
            json:json_read_dict(Stream, Info, [value_string_as(atom)])
        ;
            throw(error(extension_not_found(Name)))).

    :- public(get_extensions/1).
    :- info(get_extensions/1, [
        comment is 'Get list of extensions.'
    ]).
    get_extensions(Extensions) :-
        findall(P, (expand_file_search_path(extension(.), P)), Paths),
        meta::map(directory_files, Paths, Files),
        lists:flatten(Files, FlatFiles),
        meta::map([X, Y]>>(atom_concat(X, '/extension', Y)), FlatFiles, Candidates),
        meta::map({A, ExtFile, Stream}/[X, Y]>>(findall(A,
            (
                absolute_file_name(extension(X), ExtFile, [
                    extensions([json]),
                    solutions(all)
                ]),
                exists_file(ExtFile),
                open(ExtFile, read, Stream),
                json:json_read_dict(Stream, Dict, [value_string_as(atom)]),
                select_dict(_{name: A}, Dict, _)
            ), Y)
        ), Candidates, Found),
        lists:flatten(Found, Extensions).

    :- public(get_extensions_dict/1).
    :- info(get_extensions_dict/1, [
        comment is 'Get list of extension info dicts.'
    ]).
    get_extensions_dict(Extensions) :-
        model::new(ExtDb, [name(extension)]),
        findall(P, (expand_file_search_path(extension(.), P)), Paths),
        meta::map(directory_files, Paths, Files),
        lists:flatten(Files, FlatFiles),
        meta::map([X, Y]>>(atom_concat(X, '/extension', Y)), FlatFiles, Candidates),
        meta::map({E, ExtFile, Stream, ExtDb}/[X, Y]>>(findall(E,
            (
                absolute_file_name(extension(X), ExtFile, [
                    extensions([json]),
                    solutions(all)
                ]),
                exists_file(ExtFile),
                open(ExtFile, read, Stream),
                json:json_read_dict(Stream, Dict, [value_string_as(atom)]),
                select_dict(_{name: Name}, Dict, _),
                (ExtDb::exec(current, [Name, _]) ->
                    put_dict(Dict, ins_dict{installed: true}, InsDict)
                ;
                    put_dict(Dict, ins_dict{installed: false}, InsDict)
                ),
                (pcre:re_match("/core/extension/", ExtFile) ->
                    put_dict(InsDict, core_dict{core: true}, CoreDict)
                ;
                    put_dict(InsDict, core_dict{core: false}, CoreDict)
                ),
                (\+(check{required: _} :< CoreDict) ->
                    put_dict(CoreDict, req_dict{required: false}, E)
                ;
                    select_dict(_{required: Req}, CoreDict, _),
                    put_dict(CoreDict, req_dict{required: Req}, E))
            ), Y)
        ), Candidates, Found),
        lists:flatten(Found, Unsorted),
        sort(name, @=<, Unsorted, Extensions).

:- end_object.