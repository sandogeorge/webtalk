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

:- object(config_tests,
    extends(lgtunit)).

    :- info([
        version is 1:1:0,
        author is 'Sando George',
        date is 2018-12-24,
        comment is 'Test configuration object.'
    ]).

    succeeds(extend_config) :-
        app_settings::new(Config, []),
        assertz(user:app_settings(Config)),
        abolish_object(Config).

    succeeds(override_property) :-
        app_settings::new(Config, []),
        Config::config_property(server_port, Port),
        Port == 5000,
        Config::retractall(config_property(server_port, _)),
        Config::assertz(config_property(server_port, 4000)),
        Config::config_property(server_port, NewPort),
        NewPort == 4000,
        abolish_object(Config).

:- end_object.