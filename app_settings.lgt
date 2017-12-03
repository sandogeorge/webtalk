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

%% Application configuration.

% Directory paths.
:- prolog_load_context(directory, Dir),
    string_concat(Dir, "/..", BaseDir),
    assertz(user:file_search_path(base, BaseDir)),
    %% Data directory.
    assertz(user:file_search_path(data, base('data'))),
    %% App directory.
    assertz(user:file_search_path(app, base('app'))),
    assertz(user:file_search_path(theme, app('theme'))),
    assertz(user:file_search_path(extension, app('extension'))),
    %% Core directory.
    assertz(user:file_search_path(core, base('core'))),
    assertz(user:file_search_path(blueprint, core('blueprint'))),
    assertz(user:file_search_path(static, blueprint('static'))),
    assertz(user:file_search_path(wellknown, blueprint('wellknown'))),
    assertz(user:file_search_path(model, core('model'))),
    assertz(user:file_search_path(theme, core('theme'))),
    assertz(user:file_search_path(extension, core('extension'))).

% app_prefix(?Prefix)
%
% The application prefix is used when importing configuration values from
% environment variables. Its main purpose is to avoid conflicts with variables
% used by other software on the system.
:- multifile user:app_prefix/1.
:- dynamic user:app_prefix/1.
app_prefix('WEBTALK_').

% app_settings(?Settings).
%
% The application configuration is a value representing the deployment
% environment of the application. It should be one of `development`, `testing`
% and `production` in order to be consistent with the configuration objects
% declared in this file. The use of deployment specific configuration objects
% contributes to a smooth integration and deployment workflows from development
% machines, to production servers. This value is set during loading.
:- multifile user:app_settings/1.
:- dynamic user:app_settings/1.

% Configuration objects.
%
% The values defined in the `config` object are overriden, if necessary, in the
% deployment specific configuration objects.
:- object(app_settings).

    :- info([
        version is 3.1,
        author is 'Sando George',
        date is 2017/10/26,
        comment is 'Base configuration object.'
    ]).

    :- public(new/2).
    new(Instance, Clauses) :-
        self(Self),
        create_object(Instance, [extends(Self)], [], Clauses),
        Instance::init.

    :- public(config_property/2).
    :- dynamic(config_property/2).
    :- info(config_property/2, [
        comment is 'Set application wide configuration porperty.'
    ]).
    config_property(server_port, 5000).
    config_property(site_name, 'Webtalk').
    config_property(jquery_version, '3.2.1').
    config_property(jquery_validate_version, '1.17.0').
    config_property(popper_version, '1.11.0').
    config_property(bootstrap_version, '4.0.0-beta.2').

    :- public(daemon_option/2).
    :- dynamic(daemon_option/2).
    :- info(daemon_option/2, [
        comment is 'Set option to be passed to http_daemon.'
    ]).
    daemon_option(pidfile, '/var/run/webtalk').
    daemon_option(output, '/var/log/webtalk.log').

    :- public(daemonize/1).
    :- dynamic(daemonize/1).
    :- info(daemonize/1, [
        comment is 'Whether or not the app shoud use http_unix_daemon.'
    ]).
    daemonize(false).

    :- public(https_only/1).
    :- dynamic(https_only/1).
    :- info(https_only/1, [
        comment is 'Force use of HTTPS pages.'
    ]).
    https_only(false).

    :- private(init/0).
    :- info(init/0, [
        comment is 'Read common configs from env vars. Use defaults if empty.'
    ]).
    init :-
        init_config_properties,
        init_daemon_options.

    :- private(init_config_properties/0).
    :- info(init_config_properties/0, [
        comment is 'Initialize config vars from env vars or use defaults.'
    ]).
    init_config_properties :-
        (get_env('SERVER_PORT', ServerPort) ->
            ::assertz(config_property(server_port, ServerPort))
        ; true),
        (get_env('SITE_NAME', SiteName) ->
            ::assertz(config_property(site_name, SiteName))
        ; true),
        (get_env('JQUERY_VERSION', JQueryVersion) ->
            ::assertz(config_property(jquery_version, JQueryVersion))
        ; true),
        (get_env('JQUERY_VALIDATE_VERSION', JQueryValidateVersion) ->
            ::assertz(config_property(jquery_validate_version, JQueryValidateVersion))
        ; true),
        (get_env('POPPER_VERSION', PopperVersion) ->
            ::assertz(config_property(popper_version, PopperVersion))
        ; true),
        (get_env('BOOTSTRAP_VERSION', BootstrapVersion) ->
            ::assertz(config_property(bootstrap_version, BootstrapVersion))
        ; true).

    :- private(init_daemon_options/0).
    :- info(init_daemon_options/0, [
        comment is 'Initialize HTTP daemon options from env vars or use defaults.'
    ]).
    init_daemon_options :-
        (get_env('DAEMONIZE', Daemonize) ->
            ::assertz(daemonize(Daemonize))
        ; true),
        (get_env('DAEMON_HTTPS_ONLY', HttpsOnly) ->
            ::assertz(https_only(HttpsOnly))
        ; true),
        (get_env('DAEMON_PORT', Port) ->
            ::assertz(daemon_option(port, Port))
        ; true),
        (get_env('DAEMON_IP', IP) ->
            ::assertz(daemon_option(ip, IP))
        ; true),
        (get_env('DAEMON_DEBUG', Debug) ->
            ::assertz(daemon_option(debug, Debug))
        ; true),
        (get_env('DAEMON_SYSLOG', Syslog) ->
            ::assertz(daemon_option(syslog, Syslog))
        ; true),
        (get_env('DAEMON_USER', User) ->
            ::assertz(daemon_option(user, User))
        ; true),
        (get_env('DAEMON_GROUP', Group) ->
            ::assertz(daemon_option(group, Group))
        ; true),
        (get_env('DAEMON_PIDFILE', Pidfile) ->
            ::assertz(daemon_option(pidfile, Pidfile))
        ; true),
        (get_env('DAEMON_OUTPUT', Output) ->
            ::assertz(daemon_option(output, Output))
        ; true),
        (get_env('DAEMON_FORK', Fork) ->
            ::assertz(daemon_option(fork, Fork))
        ; true),
        (get_env('DAEMON_HTTP', Http) ->
            ::assertz(daemon_option(http, Http))
        ; true),
        (get_env('DAEMON_HTTPS', Https) ->
            ::assertz(daemon_option(https, Https))
        ; true),
        (get_env('DAEMON_CERTFILE', Certfile) ->
            ::assertz(daemon_option(certfile, Certfile))
        ; true),
        (get_env('DAEMON_KEYFILE', Keyfile) ->
            ::assertz(daemon_option(keyfile, Keyfile))
        ; true),
        (get_env('DAEMON_PWFILE', Pwfile) ->
            ::assertz(daemon_option(pwfile, Pwfile))
        ; true),
        (get_env('DAEMON_PASSWORD', Password) ->
            ::assertz(daemon_option(password, Password))
        ; true),
        (get_env('DAEMON_CIPHERLIST', Cipherlist) ->
            ::assertz(daemon_option(cipherlist, Cipherlist))
        ; true),
        (get_env('DAEMON_INTERACTIVE', Interactive) ->
            ::assertz(daemon_option(interactive, Interactive))
        ; true),
        (get_env('DAEMON_GTRACE', Gtrace) ->
            ::assertz(daemon_option(gtrace, Gtrace))
        ; true),
        (get_env('DAEMON_SIGHUP', Sighup) ->
            ::assertz(daemon_option(sighup, Sighup))
        ; true),
        (get_env('DAEMON_WORKERS', Workers) ->
            ::assertz(daemon_option(workers, Workers))
        ; true).

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

:- object(development_settings,
    extends(app_settings)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/10/19,
        comment is 'Development environment specific configuration object.'
    ]).

:- end_object.

:- object(testing_settings,
    extends(app_settings)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/10/19,
        comment is 'Testing environment specific configuration object.'
    ]).

:- end_object.

:- object(production_settings,
    extends(app_settings)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/10/19,
        comment is 'Production environment specific configuration object.'
    ]).

:- end_object.