:-module(scripts, [models_unit_sets/3
		  ,list_models_unit_sets/3]).

/** <module> Scripts to facilitate using the web api.

Predicates in this module generally call project predicates in a
sequence and handle the conversion of their arguments from the format
passed in by the json.

*/

:-use_module(src(unit)).

%!	models_unit_sets(+Models, +Name, -Unit_model_sets) is det.
%
%	Convert between a list of models and a unit's model-sets.
%
%	Models is a list of strings representing compounds
%	Model-Numbers, where Model is the name of a model and Numbers
%	the numbers of the model in the unit to be constructed.
%
%	Models is passed in as a list of atoms, as read from the json,
%	and must be converted to compounds.
%
models_unit_sets(Ms, N, Ss):-
	findall(T
	       ,(member(A,Ms)
		,term_to_atom(T,A))
	       ,Ts)
	,models_unit(Ts, N, N-Us)
	,model_sets(Us,Ss).



%!	list_model_unit_sets(+Models,+Name,-Unit) is det.
%
%	Print out a list of model-sets in a Unit.
%
%	As models_unit_sets but also pretty-prints the model sets in the
%	constructed unit.
%
list_models_unit_sets(Ms, N, Us):-
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
