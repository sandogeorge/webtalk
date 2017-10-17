% Support lambda functions.
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

% HTTP modules.
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_log)).

:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_write)).

% Utility.
:- use_module(library(memfile)).
:- use_module(library(lists)).
:- use_module(library(st/st_render)).