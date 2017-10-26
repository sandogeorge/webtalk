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


%% Abstract paths.
:- multifile http:location/3.
:- dynamic http:location/3.
http:location(static, root('static'), []).
http:location(api, root('api'), []).

%% Pre-handler goals.
:- multifile http:request_expansion/2.
http:request_expansion(_RequestIn, _RequestOut) :-
   member(protocol(P), _RequestIn),
   user:app_http_only(Bool),
   ((Bool, P == 'http') ->
        member(host(Host), _RequestIn),
        member(request_uri(Uri), _RequestIn),
        atomic_list_concat(['https://', Host, Uri], To),
        throw(http_reply(moved_temporary(To)))
   ; _RequestOut = _RequestIn).

%% Declare handlers.

% Index page.
:- http_handler(root('.'), [Request]>>(frontend::index(Request)), []).

% Static files.
:- http_handler(static('.'), [Request]>>(frontend::static(Request)), [prefix]).

% API endpoints.
:- http_handler(api('.'), [Request]>>(api::index(Request)), []).