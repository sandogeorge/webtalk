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
    [Request]>>(main::index(Request)), [id("main.index")]).

% Install page.
:- http_handler(root('install'),
    [Request]>>(main::install(Request)), [id("main.install")]).

% Authentication pages.
:- http_handler(auth('login'),
    [Request]>>(auth::login(Request)), [id("auth.login")]).

:- http_handler(auth('logout'),
    [Request]>>(auth::logout(Request)), [id("auth.logout")]).

% Static files.
:- http_handler(static('.'),
    [Request]>>(main::static(Request)), [id("static"), prefix]).

% Certbot.
:- http_handler(well_known('.'),
    [Request]>>(main::well_known(Request)), [id("well_known"), prefix]).

% API endpoints.
:- http_handler(api('.'),
    [Request]>>(api::index(Request)), [id("api.index")]).

%% Template functions.
:- st_expr:st_set_function(url_for, 1,
    [Id, Out]>>(routing::url_for(Id, Out))).

:- object(routing).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/03,
        comment is 'Defines common routing functions.'
    ]).

    :- public(redirect/2).
    :- info(redirect/2, [
        comment is 'Perform a 302 redirect.'
    ]).
    redirect(Spec, Base) :-
        http_dispatch:http_absolute_location(Spec, Url, [relative_to(Base)]),
        throw(http_reply(moved_temporary(Url))).

    :- public(handle_expansion/1).
    :- info(handle_expansion/1, [
        comment is 'Handle all request expansion checks'
    ]).
    handle_expansion(_Request) :-
        ::check_protocol(_Request),
        ::check_not_installed(_Request),
        ::check_already_installed(_Request).

    :- private(check_protocol/1).
    :- info(check_protocol/1, [
        comment is 'Check if redirect is needed from HTTP to HTTPS'
    ]).
    check_protocol(_Request) :-
        lists:member(protocol(Proto), _Request),
        user:app_config(AppConfig),
        AppConfig::https_only(Bool),
        ((Bool, Proto == 'http') ->
            lists:member(host(Host), _Request),
            lists:member(path(Path), _Request),
            atomic_list_concat(['https://', Host, Path], To),
            throw(http_reply(moved_temporary(To)))
        ; true).

    :- private(check_not_installed/1).
    :- info(check_not_installed/1, [
        comment is 'Redirect to install page if the installed flag is not set.'
    ]).
    check_not_installed(_Request) :-
        lists:member(path(Path), _Request),
        user:get_model(flag, Flag),
        ((not(Flag::exec(current, [installed, _])),
          not(pcre:re_match("^/install[/]?$", Path)),
          not(pcre:re_match("^/static", Path))) ->
            lists:member(protocol(Proto), _Request),
            lists:member(host(Host), _Request),
            lists:member(port(Port), _Request),
            http_dispatch:http_absolute_location(root('install'), Url, [relative_to(Path)]),
            atomic_list_concat([Proto, '://', Host, ':', Port, Url], To),
            throw(http_reply(moved_temporary(To)))
        ; true).

    :- private(check_already_installed/1).
    :- info(check_already_installed/1, [
        comment is 'Redirect to index page if the installed flag is set.'
    ]).
    check_already_installed(_Request) :-
        lists:member(path(Path), _Request),
        user:get_model(flag, Flag),
        ((Flag::exec(current, [installed, Installed]),
          Installed,
          pcre:re_match("^/install[/]?$", Path)) ->
            lists:member(protocol(Proto), _Request),
            lists:member(host(Host), _Request),
            lists:member(port(Port), _Request),
            http_dispatch:http_absolute_location(root('.'), Url, [relative_to(Path)]),
            atomic_list_concat([Proto, '://', Host, ':', Port, Url], To),
            throw(http_reply(moved_temporary(To)))
        ; true).

    :- public(url_for/2).
    :- info(url_for/2, [
        comment is 'Given a handler ID, return the url.'
    ]).
    url_for(Id, Out) :-
        http_dispatch:http_location_by_id(Id, Out).

:- end_object.