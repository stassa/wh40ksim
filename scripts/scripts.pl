:-module(scripts, [list_model_unit_sets/3]).

:-use_module(src(unit)).

list_model_unit_sets(Ms, N, Us):-
	findall(T
	       ,(member(A,Ms)
		,term_to_atom(T,A))
	       ,Ts)
	,models_unit(Ts, N, N-Us)
	,model_sets(Us, Ss)
	,forall(member(Si,Ss)
	       ,(writeln('Model-set:')
		,forall(member(Sk,Si)
		       ,writeln(Sk)
		       )
		)
	       ).
