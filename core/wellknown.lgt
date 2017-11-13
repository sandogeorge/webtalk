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
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).

:- object(wellknown).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/04,
        comment is 'Defines handlers for completing ACME challenges.'
    ]).

    :- public(well_known/1).
    :- info(well_known/1, [
        comment is 'Serve ACME challenges.',
        argnames is ['Request']
    ]).
    well_known(Request) :-
        lists:member(path(Path), Request),
        expand_file_search_path(app(Path), Expanded),
        exists_file(Expanded),
        http_files:http_reply_from_files(app('.well-known'), [], Request).
    well_known(Request) :-
        http_dispatch:http_404([], Request).

:- end_object.