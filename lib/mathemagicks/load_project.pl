:-prolog_load_context(directory, Dir)
,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(src, project_root(src)).

:-doc_browser.

:-use_module(src(mathemagicks)).

edit_files:-
	edit(project_root(load_project))
	,edit(src(mathemagicks))
	,edit(src(functions))
	,edit(src(regression))
	.

:-edit_files.

%:-load_test_files([]).
%:-run_tests.

% Large data may require a larger stack.
%:-set_prolog_stack(global, limit(2**9*10**6)).
:-prolog_stack_property(global, limit(X))
,format('Global stack limit ~D~n',[X]).
