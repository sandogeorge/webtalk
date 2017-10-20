# WebTalk

Welcome to WebTalk, a repository of boilerplate code for building web applications using [Logtalk](http://logtalk.org/) with [SWI-Prolog](http://www.swi-prolog.org/) as the backend.

## Features
- [Bootstrap](https://getbootstrap.com/)  based templates using the [simple-template](https://github.com/rla/simple-template) template processor for SWI-Prolog.

- Scaffolding for creating frontend HTML pages.

- Scaffolding for creating JSON based API endpoints.

## Dependencies
- SWI-Prolog >= 7.7.1

- Logtalk >= 3.12.0

- simple-template >= 1.0.1

## Configuration
All configurations are currently in `config.lgt`. Supported configurations include the server port and the site name used in the base HTML template.

## Usage (Linux)
To run the application, issue the following command in the base directory:

```bash
$ swilgt -s load.lgt
```

## Development
- Modules used throughtout the application are declared in `modules.lgt`.

- All path handlers are declared in `app/handlers.lgt`.

- Handlers for frontend HTML pages are defined in `app/frontend.lgt`.

- Handlers for API endpoints are defined in `app/api.lgt`.

## Copyright
Copyright (c) 2017 [Sando George](https://github.com/sandogeorge).

## License
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).