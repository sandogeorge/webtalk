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
:- use_module(library(pcre)).

:- object(blueprint).

    :- info([
        version is 1:0:0,
        author is 'Sando George',
        date is 2018-08-12,
        comment is 'Utility predicates for blueprint handling.'
    ]).

    :- public(autoload_custom_blueprints/0).
    :- info(autoload_custom_blueprints/0, [
        comment is 'Autoload all custom blueprints.'
    ]).
    autoload_custom_blueprints :-
        findall(P, (expand_file_search_path(blueprint(.), P)), Paths),
        meta::map(directory_files, Paths, Files),
        lists:flatten(Files, FlatFiles),
        meta::map([X, Y]>>(
            atomic_list_concat([X, '/', X], Y)
        ), FlatFiles, Candidates),
        meta::map({A}/[X, Y]>>(findall(A,
            (
                absolute_file_name(blueprint(X), A, [
                    extensions([lgt]),
                    solutions(all)
                ]),
                pcre:re_match("/app/blueprint/", A),
                exists_file(A)
            ), Y)
        ), Candidates, Found),
        lists:flatten(Found, Blueprints),
        load_blueprints(Blueprints).

    :- private(load_blueprints/1).
    :- info(load_blueprints/1, [
        comment is 'Load the supplied blueprints.'
    ]).
    load_blueprints([]).
    load_blueprints([Blueprint | Blueprints]) :-
        logtalk_load(Blueprint),
        load_blueprints(Blueprints).

:- end_object.
