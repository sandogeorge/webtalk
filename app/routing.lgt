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
    lists:member(path(Path), _RequestIn),
    ((not(pcre:re_match("^/static", Path)),
      not(pcre:re_match("^/[.]well-known", Path))) ->
        routing::handle_expansion(_RequestIn)
    ; true),
    _RequestOut = _RequestIn.

%% Declare handlers.

% Index page.
:- http_handler(root('.'),
    [Request]>>(main::index(Request)), [id("main.index")]).

% Install page.
:- http_handler(root('install'),
    [Request]>>(install::install(Request)), [id("install")]).

% Authentication pages.
:- http_handler(auth('login'),
    [Request]>>(auth::login(Request)), [id("auth.login")]).

:- http_handler(auth('logout'),
    [Request]>>(auth::logout(Request)), [id("auth.logout")]).

% Static files.
:- http_handler(static('.'),
    [Request]>>(static::static(Request)), [id("static"), prefix]).

% Certbot.
:- http_handler(well_known('.'),
    [Request]>>(wellknown::well_known(Request)), [id("well_known"), prefix]).

% API endpoints.
:- http_handler(api('.'),
    [Request]>>(api::index(Request)), [id("api.index")]).

%% Template functions.
:- st_set_function(url_for, 1,
    [Id, Out]>>(routing::url_for(Id, Out))).

:- object(routing).

    :- info([
        version is 1.0,
        author is 'Sando George',
        date is 2017/11/03,
        comment is 'Defines common routing functions.'
    ]).

    :- private(expansion_hook/2).
    :- dynamic(expansion_hook/2).
    :- info(expansion_hook/2, [
        comment is 'External goals to be called to validate requests.'
    ]).

    :- public(assert_expansion_hook/2).
    :- info(assert_expansioin_hook/2, [
        comment is 'Register expansion hooks.'
    ]).
    assert_expansion_hook(Object, Predicate) :-
        ((nonvar(Object), nonvar(Predicate)) ->
            ::asserta(expansion_hook(Object, Predicate))
        ; true).

    :- public(retract_expansion_hook/2).
    :- info(retract_expansioin_hook/2, [
        comment is 'Deregister expansion hooks.'
    ]).
    retract_expansion_hook(Object, Predicate) :-
        (nonvar(Object) ->
            ::retract(expansion_hook(Object, Predicate))
        ; true).

    :- public(handle_expansion/1).
    :- info(handle_expansion/1, [
        comment is 'Handle all request expansion checks'
    ]).
    handle_expansion(_Request) :-
        ::verify_same_origin(_Request),
        ::check_protocol(_Request),
        ::check_not_installed(_Request),
        ::check_already_installed(_Request),
        findall(X, (expansion_hook(Obj, Pred), X = [Obj, Pred]), Hooks),
        ::call_expansion_hooks(Hooks, _Request).

    :- private(verify_same_origin/1).
    :- info(verify_same_origin/1, [
        comment is 'Check headers to verify the request is same origin.'
    ]).
    verify_same_origin(_Request) :-
        ((lists:member(method(Method), _Request), Method == post) ->
            lists:member(protocol(Protocol), _Request),
            lists:member(host(Host), _Request),
            atomic_list_concat(['^', Protocol, '://', Host], Target),
            atom_string(Target, Regex),
            (lists:member(origin(Origin), _Request) ->
                (not(pcre:re_match(Regex, Origin)) ->
                    throw(http_reply(forbidden(Origin)))
                ; true)
            ; true),
            (lists:member(referer(Referer), _Request) ->
                (not(pcre:re_match(Regex, Referer)) ->
                    throw(http_reply(forbidden(Referer)))
                ; true)
            ; true)
        ; true).

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
        model::new(Flag, [name(flag)]),
        ((not(Flag::exec(current, [installed, _])),
          not(pcre:re_match("^/install[/]?$", Path))) ->
            lists:member(protocol(Proto), _Request),
            lists:member(host(Host), _Request),
            ::get_request_port(_Request, Port),
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
        model::new(Flag, [name(flag)]),
        ((Flag::exec(current, [installed, Installed]),
          Installed,
          pcre:re_match("^/install[/]?$", Path)) ->
            lists:member(protocol(Proto), _Request),
            lists:member(host(Host), _Request),
            ::get_request_port(_Request, Port),
            http_dispatch:http_absolute_location(root('.'), Url, [relative_to(Path)]),
            atomic_list_concat([Proto, '://', Host, ':', Port, Url], To),
            throw(http_reply(moved_temporary(To)))
        ; true).

    :- private(call_expansion_hooks/2).
    :- info(call_expansion_hooks/2, [
        comment is 'Pass request to all expansion hooks for validation.'
    ]).
    call_expansion_hooks([], _).
    call_expansion_hooks([Hook|Hooks], _Request) :-
        lists:nth0(0, Hook, Object),
        lists:nth0(1, Hook, Pred),
        Callable =.. [Pred, _Request],
        Object::Callable,
        call_expansion_hooks(Hooks, _Request).

    :- public(redirect/2).
    :- info(redirect/2, [
        comment is 'Perform a 302 redirect.'
    ]).
    redirect(Spec, Base) :-
        http_dispatch:http_absolute_location(Spec, Url, [relative_to(Base)]),
        throw(http_reply(moved_temporary(Url))).

    :- public(url_for/2).
    :- info(url_for/2, [
        comment is 'Given a handler ID, return the url.'
    ]).
    url_for(Id, Out) :-
        http_dispatch:http_location_by_id(Id, Out).

    :- private(get_request_port/2).
    get_request_port(Request, Port) :-
        (not(memberchk(port(_), Request)) ->
            (memberchk(protocol(http), Request) -> Port = '80' ; Port = '443')
        ; lists:member(port(Port), Request)).

:- end_object.