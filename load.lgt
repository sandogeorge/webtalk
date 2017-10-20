:- initialization((
    logtalk_load([
        'modules',
        'config',
        'app/frontend',
        'app/api',
        'app/handlers'
    ]),
    user:app_prefix(AppPrefix),
    atom_concat(AppPrefix, 'CONFIG', Envar),
    (getenv(Envar, Config) -> true ; Config = 'development'),
    atom_concat(Config, '_config', AppConfig),
    call(AppConfig::server_port(ServerPort)),
    threaded_ignore(http_server(http_dispatch, [port(ServerPort)])),
    asserta(user:app_config(AppConfig))
)).