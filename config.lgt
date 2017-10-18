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

% Server.
:- multifile user:server_port/1.
:- dynamic user:server_port/1.
server_port(5000).

% Site.
:- multifile user:site_name/1.
:- dynamic user:site_name/1.
site_name('WebTalk').

% Templates.
:- multifile user:jquery_version/1.
:- dynamic user:jquery_version/1.
jquery_version('3.2.1').

:- multifile user:popper_version/1.
:- dynamic user:popper_version/1.
popper_version('1.11.0').

:- multifile user:bootstrap_version/1.
:- dynamic user:bootstrap_version/1.
bootstrap_version('4.0.0-beta').