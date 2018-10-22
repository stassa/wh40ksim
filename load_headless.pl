:-prolog_load_context(directory, Dir)
,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(src, project_root(src)).
user:file_search_path(lib, project_root(lib)).
user:file_search_path(data, project_root(data)).
user:file_search_path(output, project_root(output)).
user:file_search_path(mathemagicks, project_root(lib/mathemagicks/src)).
user:file_search_path(web, project_root(web)).
user:file_search_path(scripts, project_root(scripts)).

% Although first four are loaded by simulation
% It's nice to have them at the top-level also.
:-use_module(src(datasheets)).
:-use_module(src(unit)).
:-use_module(src(dice)).
:-use_module(src(model)).
:-use_module(src(simulation)).
:-use_module(src(display)).
:-use_module(web(web_api)).
:-use_module(scripts(scripts)).

%:-load_test_files([]).
%:-run_tests.

% Large data may require a larger stack.
%:-set_prolog_stack(global, limit(2**9*10**6)).
:-prolog_stack_property(global, limit(X))
,format('Global stack limit ~D~n',[X]).

:- configuration:api_port(P)
  ,start_server_at(P).
