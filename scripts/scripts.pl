:-module(scripts, [models_attacks/4
		  ,list_models_attacks/3
		  ,models_unit_sets/2
		  ,list_models_unit_sets/2
		  ]).

/** <module> Scripts to facilitate using the web api.

Predicates in this module generally call project predicates in a
sequence and handle the conversion of their arguments from the format
passed in by the json.

*/

:-use_module(src(unit)).


%!	models_attacks(+Unit,+Distance,+Movement,+Attacks) is det.
%
%	Calculate the attacks possible by the models in a Unit.
%
%	Distance is the distance to the target; Movement is the unit's
%	movement, as accepted by model_set_attacks/8 (one of none,
%	standard, advance).
%
%	Attacks is a list of compounds Model-Wargear:Attacks, where
%	Model is the name of the models in a model-set in the Unit,
%	Wargear is a wargear used by Model and Attacks the number of
%	attacks calculated for that model and wargear in the entire
%	unit.
%
%	Note that the numbers represent actual attacks made, not
%	potential- e.g. the numbers of attacks for grenades etc are
%	calculated. This is according to model_set_attacks/8.
%
models_attacks(Ms,D,Mv,AS):-
	atoms_compounds(Ms, Ms_)
	,models_unit(Ms_, nil, nil-Us)
	,model_sets(Us, Ss)
	,findall(Wg_As
		,(member([M|Si], Ss)
		 ,model_value(M, wargear, Ws)
		 ,model_value(M, name, N)
		 ,findall(N-Wg:A
			 ,(member(Wg-Wn,Ws)
			  ,model_set_attacks([M|Si], Wg, D, Mv, Mn, Pa, Wa, _Mod)
			  ,number_of_attacks(Mn, Pa, Wa, Wn, A)
			  )
			 ,Wg_As)
		 )
		,AS).



%!	list_models_attacks(+Models,+Distance,+Movement) is det.
%
%	List attacks by a set of Models.
%
%	Distance is the distance to the target; Movement is the unit's
%	movement, as accepted by model_set_attacks/8 (one of none,
%	standard, advance).
%
%	Note that the numbers represent actual attacks made, not
%	potential- e.g. the numbers of attacks for grenades etc are
%	calculated. This is according to model_set_attacks/8.
%
list_models_attacks(Ms,D,Mv):-
	atoms_compounds(Ms, Ms_)
	,models_unit(Ms_, nil, nil-Us)
	,model_sets(Us, Ss)
	,forall(member([M1|Si], Ss)
	       ,(length([M1|Si], L)
		,format('Model set of ~w ~w~n',[L,M1])
		,model_value(M1, wargear, Ws)
		,forall(member(Wg-Wn,Ws)
		       ,(model_set_attacks([M1|Si], Wg, D, Mv, Mn, Pa, Wa, _Mod)
			,number_of_attacks(Mn, Pa, Wa, Wn, A)
			,format('Attacks with weapon ~w: ~w~n', [Wg,A])
			)
		       )
		)
	       ).

%!	models_unit_sets(+Models, -Unit_model_sets) is det.
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
models_unit_sets(Ms, Ss):-
	atoms_compounds(Ms, Ts)
	,models_unit(Ts, nil, nil-Us)
	,model_sets(Us,Ss).



%!	list_model_unit_sets(+Models,+Name) is det.
%
%	Print out a list of model-sets in a Unit.
%
%	As models_unit_sets but also pretty-prints the model sets in the
%	constructed unit.
%
list_models_unit_sets(Ms, N):-
	atoms_compounds(Ms, Ts)
	,models_unit(Ts, N, N-Us)
	,model_sets(Us, Ss)
	,format('Model-sets in unit ~w:~n', [N])
	,forall(nth1(I,Ss,Si)
	       ,(format('Model-set ~D~n', [I])
		,forall(member(Sk,Si)
		       ,writeln(Sk)
		       )
		)
	       ).


%!	atoms_compounds(+Atoms, -Compounds) is det.
%
%	Convert between a list of Atoms and one of Compound terms.
%
%	Use this to handle conversion between the atomic representation
%	of compound terms sent by api requests and any compound terms
%	required by project predicates. e.g. a typical use is to convert
%	between a list of key-value pairs in atomic representation to
%	actual key-value, -/2 terms.
%
atoms_compounds(Ss, Ts):-
	findall(T
	       ,(member(A,Ss)
		,term_to_atom(T,A))
	       ,Ts).
