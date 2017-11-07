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

:- object(model_tests,
    extends(lgtunit)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/01,
        comment is 'Test configuration object.'
    ]).

    succeeds(create_model_check_name_file) :-
        create_object(Model,
            [extends(model)],
            [initialization(::init)],
            [name(user)]
        ),
        Model::name(Name),
        Name == user,
        Model::file(File),
        expand_file_search_path(data('user_model.db'), FileCheck),
        File == FileCheck.

    succeeds(add_record_to_db) :-
       create_object(Model,
           [extends(model)],
           [initialization(::init)],
           [name(user)]
       ),
       Model::exec(add, [sando, 'sando@example.com', user]).

    succeeds(update_record_in_db) :-
        create_object(Model,
            [extends(model)],
            [initialization(::init)],
            [name(user)]
        ),
        Model::exec(update, [sando, 'sando@example.com', administrator]).

    succeeds(get_current_record_from_db) :-
        create_object(Model,
            [extends(model)],
            [initialization(::init)],
            [name(user)]
        ),
        Model::exec(current, [sando, Email, Role]),
        nonvar(Email),
        nonvar(Role).

    succeeds(delete_record_from_db) :-
        create_object(Model,
            [extends(model)],
            [initialization(::init)],
            [name(user)]
        ),
        Model::exec(delete, [sando]).

    fails(check_record_deleted_from_db) :-
        create_object(Model,
            [extends(model)],
            [initialization(::init)],
            [name(user)]
        ),
        Model::exec(current, [sando, _, _]).

:- end_object.