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

:- use_module(library(pairs)).

:- object(ext_menu,
    extends(extension)).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/15,
        comment is 'Menu extension predicates.'
    ]).

    :- private(menu/1).
    :- dynamic(menu/1).

    :- private(menu_item/4).
    :- dynamic(menu_item/4).
    :- info(menu_item/4, [
        argnames is ['Menu', 'Title', 'Path', 'Weight']
    ]).

    :- public(register_menu/1).
    :- info(register_menu/1, [
        comment is 'Register a new menu.'
    ]).
    register_menu(Menu) :-
        \+(::menu(Menu)),
        ::assertz(menu(Menu)).

    :- public(deregister_menu/1).
    :- info(deregister_menu/1, [
        comment is 'Deregister an existing menu.'
    ]).
    deregister_menu(Menu) :-
        ::menu(Menu),
        ::retractall(menu_item(Menu, _, _)),
        ::retractall(menu(Menu)).

    :- public(add_menu_item/4).
    :- info(add_menu_item/4, [
        comment is 'Add a new item to a registered menu.'
    ]).
    add_menu_item(Menu, Title, Path, Weight) :-
        \+(::menu_item(Menu, Title, _, _)),
        ::assertz(menu_item(Menu, Title, Path, Weight)).

    :- public(remove_menu_item/2).
    :- info(remove_menu_item/2, [
        comment is 'Remove a menu item from a registered menu.'
    ]).
    remove_menu_item(Menu, Title) :-
        ::menu_item(Menu, Title, _, _),
        ::retractall(menu_item(Menu, Title, _, _)).

    :- public(get_menu_items/2).
    :- info(get_menu_items/2, [
        comment is 'Get all items added to a registered menu.'
    ]).
    get_menu_items(Menu, Items) :-
        findall(
            Weight-[title(Title), path(Path)],
            ::menu_item(Menu, Title, Path, Weight),
            ItemList
        ),
        keysort(ItemList, Pairs),
        pairs:pairs_values(Pairs, Items).

    :- public(get_menu_item_dicts/2).
    :- info(get_menu_item_dicts/2, [
        comment is 'Get all items added to a registered menu in dict structures.'
    ]).
    get_menu_item_dicts(Menu, Items) :-
        findall(
            Weight-_{title: Title, path: Path},
            (
                ::menu_item(Menu, Title, Path, Weight),
                ext_permission::check_path_permissions(Path)
            ),
            ItemList
        ),
        keysort(ItemList, Pairs),
        pairs:pairs_values(Pairs, Items).

    :- public(get_menu_item/3).
    :- info(get_menu_item/3, [
        comment is 'Get a specific menu item from a registered menu.'
    ]).
    get_menu_item(Menu, Title, Item) :-
        ::menu_item(Menu, Title, Path, Weight),
        Item = [title(Title), path(Path), weight(Weight)].

    :- public(inject_menus/1).
    :- info(inject_menus/1, [
        comment is 'Make menus available to templates.'
    ]).
    inject_menus(Menus) :-
        findall(Menu: _{items: Items}, (::menu(Menu), ::get_menu_item_dicts(Menu, Items)), MenuItems),
        dict_create(MenuDicts, _, MenuItems),
        Menus =  _{menus: MenuDicts}.

    install :-
        ::register_menu(main),
        ::register_menu(user),
        templating::assert_data_hook(ext_menu, inject_menus).

    uninstall :-
        true.

:- end_object.