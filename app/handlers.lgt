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
http:location(well_known, root('.well-known'), []).
http:location(auth, root('auth'), []).
http:location(api, root('api'), []).

%% Pre-handler goals.
:- multifile http:request_expansion/2.
http:request_expansion(_RequestIn, _RequestOut) :-
    routing::handle_expansion(_RequestIn),
    _RequestOut = _RequestIn.

%% Declare handlers.

% Index page.
:- http_handler(root('.'),
    [Request]>>(main::index(Request)), [id('main.index')]).

% Install page.
:- http_handler(root('install'),
    [Request]>>(main::install(Request)), [id('main.install')]).

% Authentication pages.
:- http_handler(auth('login'),
    [Request]>>(auth::login(Request)), [id('auth.login')]).

% Static files.
:- http_handler(static('.'),
    [Request]>>(main::static(Request)), [id('static'), prefix]).

% Certbot.
:- http_handler(well_known('.'),
    [Request]>>(main::well_known(Request)), [id('well_known'), prefix]).

% API endpoints.
:- http_handler(api('.'),
    [Request]>>(api::index(Request)), [id('api.index')]).