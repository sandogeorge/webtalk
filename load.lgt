:- initialization((
    logtalk_load([
        'modules',
        'config',
        'app/frontend',
        'app/api',
        'app/handlers'
    ]),
    server_port(ServerPort),
    threaded_ignore(http_server(http_dispatch, [port(ServerPort)]))
)).