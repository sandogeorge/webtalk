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
:- use_module(library(lists)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(yall)).

%% Abstract paths.
:- multifile http:location/3.
:- dynamic http:location/3.
http:location(well_known, root('.well-known'), []).

%% Certbot endpoints.
:- http_handler(well_known(.),
    [Request]>>(wellknown::well_known(Request)), [id("well_known"), prefix]).

:- object(wellknown).

    :- info([
        version is 1:0:0,
        author is 'Sando George',
        date is 2017-11-4,
        comment is 'Defines handlers for completing ACME challenges.'
    ]).

    :- public(well_known/1).
    :- info(well_known/1, [
        comment is 'Serve ACME challenges.',
        argnames is ['Request']
    ]).
    well_known(Request) :-
        lists:member(path(Path), Request),
        expand_file_search_path(wellknown(Path), Expanded),
        exists_file(Expanded),
        http_files:http_reply_from_files(wellknown('.well-known'), [], Request).
    well_known(Request) :-
        http_dispatch:http_404([], Request).

:- end_object.