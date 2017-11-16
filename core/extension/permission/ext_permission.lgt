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

:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- object(ext_permission,
    extends(extension)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/15,
        comment is 'Permission system predicates.'
    ]).

    :- use_module(clpfd, [
        (#=)/2, (#\=)/2, (#>)/2, (#<)/2,
        op(700, xfx, #=), op(700, xfx, #\=), op(700, xfx, #>), op(700, xfx, #<)
    ]).

    :- private(path_permissions/2).
    :- dynamic(path_permissions/2).
    :- info(path_permissions/2, [
        argnames is ['Path', 'Permissions']
    ]).

    :- public(set_path_permissions/2).
    :- info(set_path_permissions/2, [
        comment is 'Set access permissions for a specific path.'
    ]).
    set_path_permissions(Path, Permissions) :-
        ::retractall(path_permissions(Path, _)),
        ::assertz(path_permissions(Path, Permissions)).

    :- public(retract_path_permissions/1).
    :- info(retract_path_permissions/1, [
        comment is 'Retract access permissions for a specific path.'
    ]).
    retract_path_permissions(Path) :-
        ::retractall(path_permissions(Path, _)).

    :- public(check_path_permissions/1).
    :- info(check_path_permissions/1, [
        comment is 'Check whether or not permissions are satisfied.'
    ]).
    check_path_permissions(Path) :-
        (::path_permissions(Path, Permissions) ->
            ::validate_permissions(Permissions)
        ; true).

    :- private(validate_permissions/1).
    validate_permissions([]).
    validate_permissions([Perm | Perms]) :-
        lists:nth0(0, Perm, Object),
        lists:nth0(1, Perm, Callable),
        ((length(Perm, Len), Len #> 2) ->
            lists:nth0(2, Perm, Bool),
            (\+(Bool) ->
                \+(Object::Callable)
            ; Object::Callable)
        ; Object::Callable),
        ::validate_permissions(Perms).

    install :-
        true.

    uninstall :-
        true.

:- end_object.