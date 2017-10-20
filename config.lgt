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

% Directory paths.
:- prolog_load_context(directory, Dir),
    string_concat(Dir, "/../app", AppDir),
    asserta(user:file_search_path(app, AppDir)),
    asserta(user:file_search_path(static, app('static'))),
    asserta(user:file_search_path(template, app('templates'))).

% Abstract paths.
:- multifile http:location/3.
:- dynamic http:location/3.
http:location(static, root('static'), []).
http:location(api, root('api'), []).

:- multifile user:app_prefix/1.
:- dynamic user:app_prefix/1.
app_prefix('WEBTALK_').

:- multifile user:app_config/1.
:- dynamic user:app_config/1.

:- object(config).

    :- public([
        server_port/1,
        site_name/1,
        jquery_version/1,
        popper_version/1,
        bootstrap_version/1
    ]).
    :- dynamic([
        server_port/1,
        site_name/1,
        jquery_version/1,
        popper_version/1,
        bootstrap_version/1
    ]).

    :- initialization(init).
    :- private(init/0).
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
    get_env(Name, Value) :-
        user:app_prefix(Prefix),
        string_concat(Prefix, Name, Envar),
        getenv(Envar, Value).

:- end_object.

:- object(development_config,
    extends(config)).

:- end_object.

:- object(testing_config,
    extends(config)).

:- end_object.

:- object(production_config,
    extends(config)).

:- end_object.