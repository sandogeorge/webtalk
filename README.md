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

Webtalk supports customisation via pluggable themes and extensions. The best practice when developing a custom application is to avoid hacking the core at all costs and perform customizations via themes and extensions placed in the `app/theme` and `app/extension` directories respectively.

### Blueprints

Custom pages can be added to an application by way of custom blueprints. As a best practice, each custom blueprint should be placed in its own subdirectory of the `app/blueprint` directory, and should contain a .lgt file with the same name as the directory. For example, to create a blueprint that defines all the first level pages in your website's content structure, you could create a blueprint called `first_level` at `app/blueprint/first_level/first_level.lgt`.

To have such a blueprint overwrite core's `main` blueprint, one would just have to reassign the HTTP handlers for the endpoints in `main` to the `first_level` class. For example:

```logtalk
:- http_handler(root(.),
    [Request]>>(first_level::index(Request)), [id("first_level.index")])
```

### Themes

Each custom theme must be placed in its own subdirectory of the `app/theme` directory. The name of the subdirectory must correspond to the name of the theme and must contain, at least, a JSON theme info file, `theme.json`, and a Logtalk theme file, `theme_{{theme_name}}.lgt`. In addition to these, the theme directory must also contain a `static` subdirectory, that is used for serving static files such as CSS, JS and image files, and a `template` subdirectory that contains the HTML templates for the views that are rendered for the user. The directory tree below shows the structure of the default `base` theme that is bundled with Webtalk.

```
base
├── static
│   ├── css
│   │   ├── simple-sidebar.css
│   │   └── style.css
│   └── js
│       ├── install.js
│       └── script.js
├── template
│   ├── auth
│   │   └── login.html
│   ├── base.html
│   ├── config
│   │   ├── appearance.html
│   │   ├── extensions.html
│   │   ├── index.html
│   │   └── vendor.html
│   ├── install
│   │   └── index.html
│   └── main
│       └── index.html
├── theme_base.lgt
└── theme.json
```

The expected values for the JSON info file are:
- `name` - The name of the theme, which must match the name of the theme directory.
- `title` - The title of the theme that is displayed in configuration pages.
- `description` - A brief description of the theme that is displayed in configuration pages.
- `version` - The version number of the theme.

The `theme_{{theme_name}}.lgt` file is expected to contain an object called `theme_{{theme_name}}`, which extends the `theme` prototype provided by the core library. This object must define, at least, the `install/0` and `uninstall/0` predicates that handle setting up and tearing down theme data on installing and uninstalling the theme respectively. The example below is the theme object of the default `base` theme.

```logtalk
:- object(theme_base,
    extends(theme)).

    install :-
        {assertz(file_search_path(static, core(theme/base)))}.

    uninstall :-
        {retractall(file_search_path(static, core(theme/base)))}.

:- end_object.
```

### Extensions

Each custom extension must be placed in its own subdirectory of the `app/extension` directory. The name of the subdirectory must correspond to the name of the extension and must contain, at least, a JSON extension info file, `extension.json`, and a Logtalk extension file, `ext_{{extension_name}}.lgt`. The directory tree below shows the structure of the `menu` extension that is bundled with Webtalk.

```
menu
├── extension.json
└── ext_menu.lgt
```

The expected values for the JSON info file are:
- `name` - The name of the extension, which must match the name of the theme directory.
- `title` - The title of the extension that is displayed in configuration pages.
- `description` - A brief description of the extension that is displayed in configuration pages.
- `version` - The version number of the extension.

The `ext_{{extension_name}}.lgt` file is expected to contain an object called `ext_{{extension_name}}`, which extends the `extension` prototype provided by the core library. This object must define, at least, the `install/0` and `uninstall/0` predicates that handle setting up and tearing down extension data on installing and uninstalling the extension respectively. The example below is a snippet from the extension object of the core `menu` extension.

```logtalk
:- object(ext_menu,
    extends(extension)).

    ...

    install :-
        ::register_menu(main),
        ::register_menu(user),
        templating::assert_data_hook(ext_menu, inject_menus).

    uninstall :-
        true.

    ...

:- end_object.
```

### Menus

Webtalk supports dynamic menus via the core `menu` extension, the functions of which are accessed via the `ext_menu` object.

- To register a new menu, the `register_menu/1` predicate can be used. Two menus, `main` and `user`, are registered by default.

- Any menu created in this way can also be removed using the `deregister_menu/1` predicate.

- To add an item to a registered menu, the `add_menu_item/5` predicate can be used. The signature of this predicate is `add_menu_item(Menu, Title, Path, Weight, Description)`. The weight is an integer that affects the ascending sort order of the menu items.

- Menu items may be removed using the `remove_menu_item/2` predicate, the signature of which is `remove_menu_item(Menu, Title)`.

- All registered menus are made available to all template files via the `menus` dictionary. For example, to access the items of the `main` menu in a template file, one may use the `menus.main.items` list.

## Copyright
Copyright (c) 2017 - 2018 [Sando George](https://github.com/sandogeorge).

## License
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).
