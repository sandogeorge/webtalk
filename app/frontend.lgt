:- object(frontend).

    :- public(static/1).
    static(_Request) :-
        lists:member(path(Path), _Request),
        expand_file_search_path(app(Path), Expanded),
        exists_file(Expanded),
        http_files:http_reply_from_files(app('./static'), [], _Request).
    static(_Request) :-
        http_dispatch:http_404([], _Request).

    :- public(index/1).
    index(_Request) :-
        dict_create(Data, _, []),
        render_from_base(index, Data, 'Home', [], [], [], Render),
        format(Render).

    :- private(render_from_base/3).
    render_from_base(Template, Data, Title, AStyles, AClasses, AScripts, Render) :-
        user:site_name(Sitename),
        lists:union([
            '/static/css/style.css'
        ], AStyles, Styles),
        atom_string(Template, TStr),
        lists:union([
            TStr
        ], AClasses, Classes),
        lists:union([
            '/static/js/script.js'
        ], AScripts, Scripts),
        ::parse_template(template(Template), Data, Content),
        dict_create(BaseData, _, [
            title: Title,
            styles: Styles,
            body_classes: Classes,
            scripts: Scripts,
            page_content: Content,
            site_name: Sitename
        ]),
        ::parse_template(template(base), BaseData, Base),
        string_concat('Content-type: text/html~n~n', Base, Render).

    :- private(parse_template/3).
    parse_template(Template, Data, Content) :-
        memfile:new_memory_file(Handle),
        memfile:open_memory_file(Handle, write, Out),
        dict_create(StOptions, _, [frontend:semblance]),
        st_render:st_render_file(Template, Data, Out, StOptions),
        close(Out),
        memfile:open_memory_file(Handle, read, In, [free_on_close(true)]),
        read_string(In, _, Content),
        close(In).

:- end_object.