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

:- object(user_model_tests,
    extends(lgtunit)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/10/31,
        comment is 'Test database models for user objects.'
    ]).

    setup :-
        expand_file_search_path(data('user_db.test'), File),
        user_model:attach_user_db(File).

    succeeds(add_user) :-
        user_model:add_user(sando, 'sando@example.com', user).

    fails(add_same_user) :-
        user_model:add_user(sando, 'sando@example.com', administrator).

    succeeds(add_different_user) :-
        user_model:add_user(george, 'george@example.com', administrator).

    succeeds(get_user_role) :-
        user_model:current_user(sando, _, Role),
        Role == user.

    succeeds(change_user_role) :-
        user_model:current_user(sando, Email, _),
        user_model:update_user(sando, Email, administrator).

    succeeds(check_new_role) :-
        user_model:current_user(sando, _, Role),
        Role == administrator.

    succeeds(get_email) :-
        user_model:current_user(sando, Email, _),
        nonvar(Email).

    succeeds(delete_user) :-
        user_model:delete_user(sando).

    fails(check_user_deleted) :-
        user_model:current_user(sando, _, Role),
        var(Role).

    cleanup :-
        expand_file_search_path(data('user_db.test'), File),
        delete_file(File).

:- end_object.