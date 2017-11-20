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
:- use_module(library(apply)).
:- use_module(library(yall)).

%% Abstract paths.
:- multifile(http:location/3).
:- dynamic(http:location/3).
http:location(config, root('config'), []).

%% Configuration endpoints.
:- ext_menu::register_menu(config_core).
:- ext_menu::register_menu(config_third_party).

:- http_handler(config(.),
    [Request]>>(config::index(Request)), [id("config")]).
:- http_location_by_id("config", Loc),
    ext_menu::add_menu_item(user, 'Configuration', Loc, 50),
    ext_permission::set_path_permissions(Loc, [[auth, is_admin]]).

:- http_handler(config('appearance'),
    [Request]>>(config::appearance(Request)), [id("config.appearance")]).
:- http_location_by_id("config.appearance", Loc),
    ext_menu::add_menu_item(config_core, 'Appearance', Loc, 0),
    ext_permission::set_path_permissions(Loc, [[auth, is_admin]]).

:- http_handler(config('extensions'),
    [Request]>>(config::extensions(Request)), [id("config.extensions")]).
:- http_location_by_id("config.extensions", Loc),
    ext_menu::add_menu_item(config_core, 'Extensions', Loc, 1),
    ext_permission::set_path_permissions(Loc, [[auth, is_admin]]).

:- object(config).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/03,
        comment is 'Defines handlers for authentication pages.'
    ]).

    :- public(index/1).
    :- info(index/1, [
        comment is 'Configuration UI index page.',
        argnames is ['_Request']
    ]).
    index(_Request) :-
        dict_create(Data, _, [
            title: 'Configuration',
            styles: ['/static/css/simple-sidebar.css'],
            scripts: [],
            body_classes: ['config']
        ]),
        templating::render_from_base(config, Data, Render),
        format(Render).

    :- public(appearance/1).
    :- info(appearance/1, [
        comment is 'Configure themes.',
        argnames is ['_Request']
    ]).
    appearance(_Request) :-
        dict_create(Data, _, [
            title: 'Appearance',
            styles: ['/static/css/simple-sidebar.css'],
            scripts: [],
            body_classes: ['config.appearance']
        ]),
        templating::render_from_base(appearance, Data, Render),
        format(Render).

    :- public(extensions/1).
    :- info(extensions/1, [
        comment is 'Configure extensions.',
        argnames is ['_Request']
    ]).
    extensions(_Request) :-
        dict_create(Data, _, [
            title: 'Extensions',
            styles: ['/static/css/simple-sidebar.css'],
            scripts: [],
            body_classes: ['config.extensions']
        ]),
        templating::render_from_base(extensions, Data, Render),
        format(Render).

:- end_object.
