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

% Load application.
%
% This code:
% 1. Loads all applications source files.
% 2. Determines the deployment environment to be used and gets the appropriate
% configuration object.
% 3. Starts the HTTP server, listening on the configured server port.
% 4. Makes the correct configuration object available to the rest of the
% application.
:- initialization((
    logtalk_load([
        'file_paths',
        'modules',
        'config',
        'app/routing',
        'app/templating',
        'app/auth',
        'app/frontend',
        'app/api',
        'app/handlers'
    ]),
    logtalk_load('app/models/model'),
    user:app_prefix(AppPrefix),
    atom_concat(AppPrefix, 'CONFIG', Envar),
    (getenv(Envar, Config) -> true ; Config = 'development'),
    atom_concat(Config, '_config', AppConfig),
    asserta(user:app_config(AppConfig)),
    templating::init,
    ((AppConfig::daemonize(Bool), Bool) ->
        findall(X, (AppConfig::daemon_option(O, V), X =.. [O, V]), Options),
        use_module(library(http/http_unix_daemon)),
        http_daemon(Options)
    ;
        call(AppConfig::config_property(server_port, ServerPort)),
        threaded_ignore(http_server(http_dispatch, [port(ServerPort)])))
)).