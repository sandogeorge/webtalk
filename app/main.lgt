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

:- object(main).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/10/19,
        comment is 'Defines handlers for HTML pages.'
    ]).

    :- public(index/1).
    :- info(index/1, [
        comment is 'Index page (/).',
        argnames is ['_Request']
    ]).
    index(_Request) :-
        dict_create(Data, _, [
            title: 'Home',
            styles: [],
            scripts: [],
            body_classes: ['index']
        ]),
        templating::render_from_base(index, Data, Render),
        format(Render).

:- end_object.