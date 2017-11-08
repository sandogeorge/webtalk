# Webtalk

Welcome to Webtalk, a framework for building web applications using [Logtalk](http://logtalk.org/) with [SWI-Prolog](http://www.swi-prolog.org/) as the backend. Templating features are provided by the [simple_template](http://www.swi-prolog.org/pack/list?p=simple_template) SWI-Prolog pack.

## Dependencies
- SWI-Prolog >= 7.7.1

- Logtalk >= 3.12.0

- simple-template >= 1.2.0

## Configuration
All configurations, set using `config_property/2` or `daemon_option/2`, are currently in `config.lgt` and can be adjusted using environment variables. `daemon_option/2` is only to be used to store options related to running the server as a unix daemon. Environment variables are expected to be prefixed by the atom set using `app_prefix/1` in `config.lgt`. The default prefix is `WEBTALK_`. The environment variables that are currently supported are detailed below.

#### `config_property/2` related:

All of these options are passed to the templating system to be made available in template files.

- `WEBTALK_SERVER_PORT` - configures `server_port`, which is the port used when the application run using `http_server/2` instead of `http_daemon/1`. Default is `5000`.

- `WEBTALK_SITE_NAME` - configures `site_name`, which is the name of the site displayed in the navigation bar when using the default theme. Default is `Webtalk`.

- `WEBTALK_JQUERY_VERSION` - configures `jquery_version`, which is the version of the [jQuery](https://jquery.com/) library used in the default theme. Default is `3.2.1`.

- `WEBTALK_JQUERY_VALIDATE_VERSION` - configures `jquery_validate_version`, which is the version of the [jQuery validate](https://jqueryvalidation.org/) library used in the default theme. Default is `1.17.0`.

- `WEBTALK_POPPER_VERSION` - configures `popper_version`, which is the version of the [Popper.js](https://popper.js.org/) library used in the default theme. Default is `1.11.0`.

- `WEBTALK_BOOTSTRAP_VERSION` - configures `bootstrap_version`, which is the version of the [Bootstrap](http://getbootstrap.com/) responsive framework that the default theme is based on. Default is `4.0.0-beta`.

#### `daemon_option/2` related:

The details of all these options, except for `daemonize` and `daemon_https_only` can be found on the [http_daemon/0 documention page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0) on the SWI-Prolog website.

- `WEBTALK_DAEMONIZE` - configures `daemonize`, which is a boolean value that inicates if the application is to be run using `http_daemon/1` instead of `http_server/2`. Default is `false`.

- `WEBTALK_DAEMON_HTTPS_ONLY` - configures `https_only`, which, if set to true, forces all HTTP requests to be redirected to HTTPS. Default is `false`.

- `WEBTALK_DAEMON_PORT` - configures `daemon_port`, which is `--port` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_IP` - configures `daemon_ip`, which is `--ip` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_DEBUG` - configures `daemon_debug`, which is `--debug` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_SYSLOG` - configures `daemon_syslog`, which is `--syslog` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_USER` - configures `daemon_user`, which is `--user` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_GROUP` - configures `daemon_group`, which is `--group` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_PIDFILE` - configures `daemon_pidfile`, which is `--pidfile` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). Default is `/var/run/webtalk`.

- `WEBTALK_DAEMON_OUTPUT` - configures `daemon_output`, which is `--ouput` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). Default is `/var/log/webtalk.log`.

- `WEBTALK_DAEMON_FORK` - configures `daemon_fork`, which is `--fork` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_HTTP` - configures `daemon_http`, which is `--http` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_HTTPS` - configures `daemon_https`, which is `--https` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_CERTFILE` - configures `daemon_certfile`, which is `--certfile` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_KEYFILE` - configures `daemon_keyfile`, which is `--keyfile` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_PWFILE` - configures `daemon_pwfile`, which is `--pwfile` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_PASSWORD` - configures `daemon_password`, which is `--password` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_CIPHERLIST` - configures `daemon_cipherlist`, which is `--cipherlist` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_INTERACTIVE` - configures `daemon_interactive`, which is `--interactive` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_GTRACE` - configures `daemon_gtrace`, which is `--gtrace` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_SIGHUP` - configures `daemon_sighup`, which is `--sighup` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

- `WEBTALK_DAEMON_WORKERS` - configures `daemon_workers`, which is `--workers` on the [documentation page](http://www.swi-prolog.org/pldoc/man?predicate=http_daemon/0). If unset, the package default is used.

## Usage (Linux)

The following commands are to be issued in the base directory:

#### `http_server/2` mode:

```bash
$ swilgt -s app.lgt
```

#### `http_daemon/1` mode:

Environment variables may precede the command.

```bash
$ WEBTALK_DAEMONIZE=true sudo -E bash -c 'swilgt -s app.lgt'
```

## Running Webtalk as a service

For an example on how to run Webtalk as a systemd service, see the [service template file](https://github.com/sandogeorge/webtalk-ops/blob/master/chef/site-cookbooks/webtalk/templates/default/webtalk.erb) of the [webtalk-ops](https://github.com/sandogeorge/webtalk-ops) repository.

## Development
- Modules used throughtout the application are declared in `modules.lgt`.

- All path handlers are declared in `app/routing.lgt`.

- Handlers for frontend HTML pages are defined in `app/main.lgt`.

- Handlers for API endpoints are defined in `app/api.lgt`.

## Copyright
Copyright (c) 2017 [Sando George](https://github.com/sandogeorge).

## License
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).