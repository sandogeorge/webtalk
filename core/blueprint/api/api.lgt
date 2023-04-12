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

:- use_module(library(apply_macros)).
:- use_module(library(http/http_json)).
:- use_module(library(yall)).

%% Abstract paths.
:- multifile http:location/3.
:- dynamic http:location/3.
http:location(api, root('api'), []).

%% API endpoints.
:- http_handler(api('.'),
    [Request]>>(api::index(Request)), [id("api.index")]).

:- object(api).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017-10-19,
        comment is 'Defines handlers for RESTful API endpoints.'
    ]).

    :- public(index/1).
    :- info(index/1, [
        comment is 'Index page (/).',
        argnames is ['_Request']
    ]).
    index(_Request) :-
        dict_create(Data, _, [status: 'OK']),
        ::write_json(Data).

    :- private(write_json/1).
    :- info(write_json/1, [
        comment is 'Utility function for outputting JSON formatted data.',
        argnames is ['Data']
    ]).
    write_json(Data) :-
        format('Content-type: application/json~n~n'),
        current_output(Out),
        http_json:json_write(Out, Data).

:- end_object.