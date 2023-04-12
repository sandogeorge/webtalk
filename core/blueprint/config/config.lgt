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
:- use_module(library(http/http_parameters)).
:- use_module(library(option)).
:- use_module(library(yall)).

%% Abstract paths.
:- multifile(http:location/3).
:- dynamic(http:location/3).
http:location(config, root('config'), []).

%% Configuration endpoints.
:- user:ext_menu::register_menu(config_core).
:- user:ext_menu::register_menu(config_vendor).

:- http_handler(config(.),
    [Request]>>(config::index(Request)), [id('config')]).
:- http_location_by_id('config', Loc),
    ext_menu::add_menu_item(user, 'Configuration', Loc, 50, ''),
    ext_permission::set_path_permissions(Loc, [[auth, is_admin]]).

:- http_handler(config('appearance'),
    [Request]>>(config::appearance(Request)), [id('config.appearance')]).
:- http_location_by_id('config.appearance', Loc),
    ext_menu::add_menu_item(config_core, 'Appearance', Loc, 0, 'Adjust look \c
    and feel.'),
    ext_permission::set_path_permissions(Loc, [[auth, is_admin]]).

:- http_handler(config('extensions'),
    [Request]>>(config::extensions(Request)), [id('config.extensions')]).
:- http_location_by_id('config.extensions', Loc),
    ext_menu::add_menu_item(config_core, 'Extensions', Loc, 1, 'Manage \c
    extensions.'),
    ext_permission::set_path_permissions(Loc, [[auth, is_admin]]).

:- http_handler(config('vendor'),
    [Request]>>(config::vendor(Request)), [id('config.vendor')]).
:- http_location_by_id('config.vendor', Loc),
    ext_permission::set_path_permissions(Loc, [[auth, is_admin]]).

:- object(config).

    :- info([
        version is 1:0:0,
        author is 'Sando George',
        date is 2017-11-3,
        comment is 'Defines handlers for authentication pages.'
    ]).

    :- public(index/1).
    :- info(index/1, [
        comment is 'Configuration UI index page.',
        argnames is ['_Request']
    ]).
    index(_Request) :-
        dict_create(Data, _, [
            config: true,
            title: 'Configuration',
            styles: ['/static/css/simple-sidebar.css'],
            scripts: [],
            body_classes: ['config'],
            page_header: 'Core configuration'
        ]),
        templating::render_from_base('config/index', Data, Render),
        format(Render).

    :- public(vendor/1).
    :- info(vendor/1, [
        comment is 'Configuration UI vendor page.',
        argnames is ['_Request']
    ]).
    vendor(_Request) :-
        dict_create(Data, _, [
            config: true,
            title: 'Vendor',
            styles: ['/static/css/simple-sidebar.css'],
            scripts: [],
            body_classes: ['config', 'vendor'],
            page_header: 'Vendor configuration'
        ]),
        templating::render_from_base('config/vendor', Data, Render),
        format(Render).

    :- public(appearance/1).
    :- info(appearance/1, [
        comment is 'Configure themes.',
        argnames is ['Request']
    ]).
    :- meta_predicate(http_parameters:http_parameters(*, *, *)).
    appearance(Request) :-
        list::member(method(Method), Request),
        (Method == 'post' ->
            model::new(ThemeDb, [name(theme)]),
            theme_manager::get_themes(Themes),
            http_parameters:http_parameters(Request, [], [form_data(Data)]),
            meta::map([X]>>(
                atom_concat(X, '_theme', ThemeField),
                Member =.. [ThemeField, Enabled],
                ((swi_option:option(Member, Data, off)) ->
                    ((Enabled == 'on', \+ ThemeDb::exec(current, [X, _])) ->
                        theme_manager::install_theme(X)
                    ; true),
                    ((Enabled == 'off', ThemeDb::exec(current, [X, IsDefault]), \+ IsDefault) ->
                        theme_manager::uninstall_theme(X)
                    ; true)
                ; true)
            ), Themes),
            swi_option:option(default_theme(Default), Data),
            theme_manager::set_default(Default),
            templating::flash('Changes saved.', 'success'),
            list::member(path(Base), Request),
            routing::redirect(config('appearance'), Base)
        ;
            theme_manager::get_themes_dict(Themes),
            dict_create(Data, _, [
                config: true,
                title: 'Appearance',
                styles: ['/static/css/simple-sidebar.css'],
                scripts: [],
                body_classes: ['config.appearance'],
                page_header: 'Appearance',
                themes: Themes
            ]),
            templating::render_from_base('config/appearance', Data, Render),
            format(Render)).

    :- public(extensions/1).
    :- info(extensions/1, [
        comment is 'Configure extensions.',
        argnames is ['Request']
    ]).
    extensions(Request) :-
        list::member(method(Method), Request),
        (Method == 'post' ->
            model::new(ExtDb, [name(extension)]),
            extension_manager::get_extensions(Extensions),
            http_parameters:http_parameters(Request, [], [form_data(Data)]),
            meta::map([X]>>(
                atom_concat(X, '_ext', ExtField),
                Member =.. [ExtField, Enabled],
                ((swi_option:option(Member, Data, off)) ->
                    ((Enabled == 'on', \+ ExtDb::exec(current, [X, _])) ->
                        extension_manager::install_extension(X)
                    ; true),
                    ((Enabled == 'off', ExtDb::exec(current, [X, Required]), Required \== 'true') ->
                        extension_manager::uninstall_extension(X)
                    ; true)
                ; true)
            ), Extensions),
            templating::flash('Changes saved.', 'success'),
            list::member(path(Base), Request),
            routing::redirect(config('extensions'), Base)
        ;
            extension_manager::get_extensions_dict(Exts),
            dict_create(Data, _, [
                config: true,
                title: 'Extensions',
                styles: ['/static/css/simple-sidebar.css'],
                scripts: [],
                body_classes: ['config.extensions'],
                page_header: 'Extensions',
                extensions: Exts
            ]),
            templating::render_from_base('config/extensions', Data, Render),
            format(Render)).

:- end_object.
