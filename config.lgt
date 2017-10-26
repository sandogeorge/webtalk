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
        version is 3.1,
        author is 'Sando George',
        date is 2017/10/26,
        comment is 'Base configuration object.'
    ]).

    :- public(config_property/2).
    :- dynamic(config_property/2).
    :- info(config_property/2, [
        comment is 'Set application wide configuration porperty.'
    ]).
    config_property(server_port, 5000).
    config_property(site_name, 'Webtalk').
    config_property(jquery_version, '3.2.1').
    config_property(popper_version, '1.11.0').
    config_property(bootstrap_version, '4.0.0-beta').

    :- public(daemon_option/2).
    :- dynamic(daemon_option/2).
    :- info(daemon_option/2, [
        comment is 'Set option to be passed to http_daemon.'
    ]).
    daemon_option(pidfile, '/var/run/webtalk').
    daemon_option(output, '/var/log/webtalk.log').

    :- public(daemonize/1).
    :- info(daemonize/1, [
        comment is 'Whether or not the app shoud use http_unix_daemon.'
    ]).
    daemonize(false).

    :- public(http_only/1).
    :- info(http_only/1, [
        comment is 'Force use of HTTPS pages.'
    ]).
    http_only(false).

    :- initialization(init).
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
            ::asserta(config_property(server_port, ServerPort))
        ; true),
        (get_env('SITE_NAME', SiteName) ->
            ::asserta(config_property(site_name, SiteName))
        ; true),
        (get_env('JQUERY_VERSION', JQueryVersion) ->
            ::asserta(config_property(jquery_version, JQueryVersion))
        ; true),
        (get_env('POPPER_VERSION', PopperVersion) ->
            ::asserta(config_property(popper_version, PopperVersion))
        ; true),
        (get_env('BOOTSTRAP_VERSION', BootstrapVersion) ->
            ::asserta(config_property(bootstrap_version, BootstrapVersion))
        ; true).

    :- private(init_daemon_options/0).
    :- info(init_daemon_options/0, [
        comment is 'Initialize HTTP daemon options from env vars or use defaults.'
    ]).
    init_daemon_options :-
        (get_env('DAEMON_PORT', Port) ->
            ::asserta(daemon_option(port, Port))
        ; true),
        (get_env('DAEMON_IP', IP) ->
            ::asserta(daemon_option(ip, IP))
        ; true),
        (get_env('DAEMON_DEBUG', Debug) ->
            ::asserta(daemon_option(debug, Debug))
        ; true),
        (get_env('DAEMON_SYSLOG', Syslog) ->
            ::asserta(daemon_option(syslog, Syslog))
        ; true),
        (get_env('DAEMON_USER', User) ->
            ::asserta(daemon_option(user, User))
        ; true),
        (get_env('DAEMON_GROUP', Group) ->
            ::asserta(daemon_option(group, Group))
        ; true),
        (get_env('DAEMON_PIDFILE', Pidfile) ->
            ::asserta(daemon_option(pidfile, Pidfile))
        ; true),
        (get_env('DAEMON_OUTPUT', Output) ->
            ::asserta(daemon_option(output, Output))
        ; true),
        (get_env('DAEMON_FORK', Fork) ->
            ::asserta(daemon_option(fork, Fork))
        ; true),
        (get_env('DAEMON_HTTP', Http) ->
            ::asserta(daemon_option(http, Http))
        ; true),
        (get_env('DAEMON_HTTPS', Https) ->
            ::asserta(daemon_option(https, Https))
        ; true),
        (get_env('DAEMON_CERTFILE', Certfile) ->
            ::asserta(daemon_option(certfile, Certfile))
        ; true),
        (get_env('DAEMON_KEYFILE', Keyfile) ->
            ::asserta(daemon_option(keyfile, Keyfile))
        ; true),
        (get_env('DAEMON_PWFILE', Pwfile) ->
            ::asserta(daemon_option(pwfile, Pwfile))
        ; true),
        (get_env('DAEMON_PASSWORD', Password) ->
            ::asserta(daemon_option(password, Password))
        ; true),
        (get_env('DAEMON_CIPHERLIST', Cipherlist) ->
            ::asserta(daemon_option(cipherlist, Cipherlist))
        ; true),
        (get_env('DAEMON_INTERACTIVE', Interactive) ->
            ::asserta(daemon_option(interactive, Interactive))
        ; true),
        (get_env('DAEMON_GTRACE', Gtrace) ->
            ::asserta(daemon_option(gtrace, Gtrace))
        ; true),
        (get_env('DAEMON_SIGHUP', Sighup) ->
            ::asserta(daemon_option(sighup, Sighup))
        ; true),
        (get_env('DAEMON_WORKERS', Workers) ->
            ::asserta(daemon_option(workers, Workers))
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