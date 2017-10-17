% Declare handlers.
:- http_handler(root('.'), [Request]>>(frontend::index(Request)), []).
:- http_handler(static('.'), [Request]>>(frontend::static(Request)), [prefix]).
:- http_handler(api('.'), [Request]>>(api::index(Request)), []).