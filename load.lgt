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

:- initialization((
    logtalk_load([
        'modules',
        'config',
        'app/frontend',
        'app/api',
        'app/handlers'
    ]),
    user:app_prefix(AppPrefix),
    atom_concat(AppPrefix, 'CONFIG', Envar),
    (getenv(Envar, Config) -> true ; Config = 'development'),
    atom_concat(Config, '_config', AppConfig),
    call(AppConfig::server_port(ServerPort)),
    threaded_ignore(http_server(http_dispatch, [port(ServerPort)])),
    asserta(user:app_config(AppConfig))
)).