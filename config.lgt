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

%% Directory paths.
:- prolog_load_context(directory, Dir),
    string_concat(Dir, "/../app", AppDir),
    asserta(user:file_search_path(app, AppDir)),
    asserta(user:file_search_path(static, app('static'))),
    asserta(user:file_search_path(template, app('templates'))).

%% Application configuration.

% app_prefix(?Prefix)
%
% The application prefix is used when importing configuration values from
% environment variables. Its main purpose is to avoid conflicts with variables
% used by other software on the system.
:- multifile user:app_prefix/1.
:- dynamic user:app_prefix/1.
app_prefix('WEBTALK_').

% app_config(?Config).
%
% The application configuration is a value representing the deployment
% environment of the application. It should be one of `development`, `testing`
% and `production` in order to be consistent with the configuration objects
% declared in this file. The use of deployment specific configuration objects
% contributes to a smooth integration and deployment workflows from development
% machines, to production servers. This value is set during loading.
:- multifile user:app_config/1.
:- dynamic user:app_config/1.

% Configuration objects.
%
% The values defined in the `config` object are overriden, if necessary, in the
% deployment specific configuration objects.
:- object(config).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/10/19,
        comment is 'Base configuration object.'
    ]).

    :- public(server_port/1).
    :- dynamic(server_port/1).
    :- info(server_port/1, [
        comment is 'Network port that the application will listen on.',
        argnames is ['ServerPort'],
        redefinition is 'specialize'
    ]).

    :- public(site_name/1).
    :- dynamic(site_name/1).
    :- info(site_name/1, [
        comment is 'Name of site as displayed in the naviagtion bar.',
        argnames is ['SiteName'],
        redefinition is 'specialize'
    ]).

    :- public(jquery_version/1).
    :- dynamic(jquery_version/1).
    :- info(jquery_version/1, [
        comment is 'jQuery version used in HTML pages.',
        argnames is ['JQueryVersion'],
        redefinition is 'specialize'
    ]).

    :- public(popper_version/1).
    :- dynamic(popper_version/1).
    :- info(popper_version/1, [
        comment is 'Popper.js version used in HTML pages.',
        argnames is ['PopperVersion'],
        redefinition is 'specialize'
    ]).

    :- public(bootstrap_version/1).
    :- dynamic(bootstrap_version/1).
    :- info(bootstrap_version/1, [
        comment is 'Bootstrap version used in HTML pages.',
        argnames is ['BootstrapVersion'],
        redefinition is 'specialize'
    ]).

    :- initialization(init).
    :- private(init/0).
    :- info(init/0, [
        comment is 'Read common configs from env vars. Use defaults if empty.'
    ]).
    init :-
        (get_env('SERVER_PORT', ServerPort) ->
            ::asserta(server_port(ServerPort))
        ;
            ::asserta(server_port(5000))),
        (get_env('SITE_NAME', SiteName) ->
            ::asserta(site_name(SiteName))
        ;
            ::asserta(site_name('WebTalk'))),
        (get_env('JQUERY_VERSION', JQueryVersion) ->
            ::asserta(jquery_version(JQueryVersion))
        ;
            ::asserta(jquery_version('3.2.1'))),
        (get_env('POPPER_VERSION', PopperVersion) ->
            ::asserta(popper_version(PopperVersion))
        ;
            ::asserta(popper_version('1.11.0'))),
        (get_env('BOOTSTRAP_VERSION', BootstrapVersion) ->
            ::asserta(bootstrap_version(BootstrapVersion))
        ;
            ::asserta(bootstrap_version('4.0.0-beta'))).

    :- private(get_env/2).
    :- info(get_env/2, [
        comment is 'Append prefix to variable name and attempt to get value.',
        argnames is ['Name', 'Value']
    ]).
    get_env(Name, Value) :-
        user:app_prefix(Prefix),
        string_concat(Prefix, Name, Envar),
        getenv(Envar, Value).

:- end_object.

:- object(development_config,
    extends(config)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/10/19,
        comment is 'Development environment specific configuration object.'
    ]).

:- end_object.

:- object(testing_config,
    extends(config)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/10/19,
        comment is 'Testing environment specific configuration object.'
    ]).

:- end_object.

:- object(production_config,
    extends(config)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/10/19,
        comment is 'Production environment specific configuration object.'
    ]).

:- end_object.