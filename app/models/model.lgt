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

:- object(model).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/01,
        comment is 'Common object to access persistent models.'
    ]).

    :- public(name/1).
    :- dynamic(name/1).
    :- info(name/1, [
        comment is 'Name of model to be used.'
    ]).

    :- public(file/1).
    :- dynamic(file/1).
    :- info(file/1, [
        comment is 'File to use as persistent database.'
    ]).

    :- public(init/0).
    init :-
        get_model(Model),
        atom_concat(Model, '.db', DbName),
        expand_file_search_path(data(DbName), File),
        ::attach_db(File),
        ::asserta(file(File)).

    :- public(exec/2).
    :- info(exec/2, [
        comment is 'Execute operations against persistency models.'
    ]).
    exec(Op, Args) :-
        ::name(Name),
        atomic_list_concat([Op, '_', Name], Call),
        lists:append([Call], Args, CallList),
        Callable =.. CallList,
        get_model(Model),
        Model:Callable.

    :- private(attach_db/1).
    :- info(attach_db/1, [
        comment is 'Attach database file.'
    ]).
    attach_db(File) :-
        ::name(Name),
        ::get_model(Model),
        atomic_list_concat(['attach_', Name, '_db'], Pred),
        Attach =.. [Pred, File],
        Model:Attach.

    :- private(get_model/1).
    :- info(get_model/1, [
        comment is 'Construct atom representing the model to be used.'
    ]).
    get_model(Model) :-
        ::name(Name),
        atom_concat(Name, '_model', Model).

:- end_object.