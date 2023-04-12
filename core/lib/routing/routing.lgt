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

:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

%% Pre-handler goals.
:- multifile http:request_expansion/2.
http:request_expansion(RequestIn, RequestOut) :-
    lists:member(path(Path), RequestIn),
    ((\+(pcre:re_match("^/static", Path)),
      \+(pcre:re_match("^/[.]well-known", Path))) ->
        routing::handle_expansion(RequestIn)
    ; true),
    RequestOut = RequestIn.

:- object(routing).

    :- info([
        version is 1:0:0,
        author is 'Sando George',
        date is 2017-11-03,
        comment is 'Defines common routing functions.'
    ]).

    :- private(expansion_hook/2).
    :- dynamic(expansion_hook/2).
    :- info(expansion_hook/2, [
        comment is 'External goals to be called to validate requests.'
    ]).

    :- public(assert_expansion_hook/2).
    :- info(assert_expansion_hook/2, [
        comment is 'Register expansion hooks.'
    ]).
    assert_expansion_hook(Object, Predicate) :-
        ((nonvar(Object), nonvar(Predicate)) ->
            ::assertz(expansion_hook(Object, Predicate))
        ; true).

    :- public(retract_expansion_hook/2).
    :- info(retract_expansion_hook/2, [
        comment is 'Deregister expansion hooks.'
    ]).
    retract_expansion_hook(Object, Predicate) :-
        (nonvar(Object) ->
            ::retractall(expansion_hook(Object, Predicate))
        ; true).

    :- public(handle_expansion/1).
    :- info(handle_expansion/1, [
        comment is 'Handle all request expansion checks'
    ]).
    handle_expansion(Request) :-
        ::check_path_access(Request),
        ::verify_same_origin(Request),
        ::check_protocol(Request),
        ::check_not_installed(Request),
        ::check_already_installed(Request),
        findall(X, (expansion_hook(Obj, Pred), X = [Obj, Pred]), Hooks),
        ::call_expansion_hooks(Hooks, Request).

    :- private(verify_same_origin/1).
    :- info(verify_same_origin/1, [
        comment is 'Check headers to verify the request is same origin.'
    ]).
    verify_same_origin(Request) :-
        ((lists:member(method(Method), Request), Method == post) ->
            lists:member(protocol(Protocol), Request),
            lists:member(host(Host), Request),
            atomic_list_concat(['^', Protocol, '://', Host], Target),
            atom_string(Target, Regex),
            (lists:member(origin(Origin), Request) ->
                (\+(pcre:re_match(Regex, Origin)) ->
                    throw(http_reply(forbidden(Origin)))
                ; true)
            ; true),
            (lists:member(referer(Referer), Request) ->
                (\+(pcre:re_match(Regex, Referer)) ->
                    throw(http_reply(forbidden(Referer)))
                ; true)
            ; true)
        ; true).

    :- private(check_protocol/1).
    :- info(check_protocol/1, [
        comment is 'Check if redirect is needed from HTTP to HTTPS'
    ]).
    check_protocol(Request) :-
        lists:member(protocol(Proto), Request),
        user:app_settings(AppSettings),
        AppSettings::https_only(Bool),
        ((Bool, Proto == 'http') ->
            lists:member(host(Host), Request),
            lists:member(path(Path), Request),
            atomic_list_concat(['https://', Host, Path], To),
            throw(http_reply(moved_temporary(To)))
        ; true).

    :- private(check_not_installed/1).
    :- info(check_not_installed/1, [
        comment is 'Redirect to install page if the installed flag is not set.'
    ]).
    check_not_installed(Request) :-
        lists:member(path(Path), Request),
        model::new(Flag, [name(flag)]),
        ((\+(Flag::exec(current, [installed, _])),
          \+(pcre:re_match("^/install[/]?$", Path))) ->
            lists:member(protocol(Proto), Request),
            lists:member(host(Host), Request),
            ::get_request_port(Request, Port),
            http_dispatch:http_absolute_location(root('install'), Url, [relative_to(Path)]),
            atomic_list_concat([Proto, '://', Host, ':', Port, Url], To),
            throw(http_reply(moved_temporary(To)))
        ; true).

    :- private(check_already_installed/1).
    :- info(check_already_installed/1, [
        comment is 'Redirect to index page if the installed flag is set.'
    ]).
    check_already_installed(Request) :-
        lists:member(path(Path), Request),
        model::new(Flag, [name(flag)]),
        ((Flag::exec(current, [installed, Installed]),
          Installed,
          pcre:re_match("^/install[/]?$", Path)) ->
            lists:member(protocol(Proto), Request),
            lists:member(host(Host), Request),
            ::get_request_port(Request, Port),
            http_dispatch:http_absolute_location(root('.'), Url, [relative_to(Path)]),
            atomic_list_concat([Proto, '://', Host, ':', Port, Url], To),
            throw(http_reply(moved_temporary(To)))
        ; true).

    :- private(check_path_access/1).
    :- info(check_path_access/1, [
        comment is 'Verify that access to the requested path is permitted.'
    ]).
    check_path_access(Request) :-
        lists:member(path(Path), Request),
        (\+(ext_permission::check_path_permissions(Path)) ->
            lists:member(protocol(Proto), Request),
            lists:member(host(Host), Request),
            ::get_request_port(Request, Port),
            http_dispatch:http_absolute_location(root('.'), Url, [relative_to(Path)]),
            atomic_list_concat([Proto, '://', Host, ':', Port, Url], To),
            throw(http_reply(moved_temporary(To)))
        ; true).

    :- private(call_expansion_hooks/2).
    :- info(call_expansion_hooks/2, [
        comment is 'Pass request to all expansion hooks for validation.'
    ]).
    call_expansion_hooks([], _).
    call_expansion_hooks([Hook|Hooks], Request) :-
        lists:nth0(0, Hook, Object),
        lists:nth0(1, Hook, Pred),
        Callable =.. [Pred, Request],
        Object::Callable,
        call_expansion_hooks(Hooks, Request).

    :- public(redirect/2).
    :- info(redirect/2, [
        comment is 'Perform a 302 redirect.'
    ]).
    redirect(Spec, Base) :-
        http_dispatch:http_absolute_location(Spec, Url, [relative_to(Base)]),
        throw(http_reply(moved_temporary(Url))).

    :- private(get_request_port/2).
    get_request_port(Request, Port) :-
        (\+(memberchk(port(_), Request)) ->
            (memberchk(protocol(http), Request) -> Port = '80' ; Port = '443')
        ; lists:member(port(Port), Request)).

:- end_object.