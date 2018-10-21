:-module(scripts, [models_allocated_wounds/3
		  ,list_models_allocated_wounds/2
		  ,hits_roll_to_wound/4
		  ,unit_roll_to_hit/4
		  ,list_unit_roll_to_hit/3
		  ,models_attacks/4
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
:-use_module(src(simulation)).



%!	models_allocated_wounds(+Models,+Wounds,-Allocated) is det.
%
%	List the Wounds allocated to the Models in a unit.
%
models_allocated_wounds(Ms,Ws,Walloc):-
	atoms_compounds(Ms, Ms_)
	,models_unit(Ms_, nil, nil-Us)
	,allocate_wounds(Ws, Us, Walloc).



%!	list_models_allocated_wounds(+Models,+Wounds) is det.
%
%	List the Wounds allocated to the Models in a unit.
%
list_models_allocated_wounds(Ms,Walloc):-
	atoms_compounds(Ms, Ms_)
	,models_unit(Ms_, nil, nil-Us)
	,allocate_wounds(Walloc, Us, MsWs)
	,forall(member(Mi-Ws, MsWs)
	       ,(model_value(Mi,'W',Wi)
		,model_value(Mi,name,Nm)
		,format('Allocated ~w wounds to ~w with ~w wounds remaining~n'
		       , [Ws, Nm, Wi])
		)
	       ).



%!	hits_roll_to_wound(+Hits,+Strength,+Toughness,-Wounds) is det.
%
%	Roll to wound for a number of Hits.
%
%	Hits is the number of hits scored by a unit, e.g. as calculated
%	by hit_roll/4. Strength is the strenght of the unit's weapons,
%	whereas Toughness is the toughness of their target. Wounds is a
%	number indicating the number of wounds rolled by the unit over
%	all Hits.
%
hits_roll_to_wound(Hn,S,T,Wn):-
	wound_roll(Hn, S, T, Wn).


%!	unit_roll_to_hit(+Models,+Distance,+Movement,-Hits) is det.
%
%	Roll to hit for all Models in a unit.
%
%	Distance is the distance of the Models in the unit to their
%	target; Movement is the movement type of the unit.
%
%	Hits is a list of lists of compounds Model-Wargear:Hits, where
%	each Model is the name of a type of model in one of the
%	modelsets the unit, Wargear is the type of wargear for models in
%	that set and Hits are the total hits rolled by that type of
%	model.
%
unit_roll_to_hit(Ms,D,Mv,Hs):-
	atoms_compounds(Ms, Ms_)
	,models_unit(Ms_, nil, nil-Us)
	,model_sets(Us, Ss)
	,findall(Nm_Wg_Hn % List of hits by model type and wargear
		,(member([M1|Si],Ss)
		 ,model_value(M1, wargear, Ws)
		 ,model_value(M1, name, Nm)
		 ,findall(Nm-Wg:Hn
			 ,(member(Wg-Wn, Ws)
			  ,model_set_attacks([M1|Si], Wg, D, Mv, Mn, Pa, Wa, _Mod)
			  ,number_of_attacks(Mn, Pa, Wa, Wn, A)
			  ,hit_roll(A, (5+), 0, Hn)
			  )
			 ,Nm_Wg_Hn)
		 )
		,Hs). % List of lists as above.



%!	list_unit_roll_to_hit(+Models,+Distance,+Movement) is det.
%
%	List the hits for Models in a unit.
%
list_unit_roll_to_hit(Ms,D,Mv):-
	atoms_compounds(Ms, Ms_)
	,models_unit(Ms_, nil, nil-Us)
	,model_sets(Us, Ss)
	,forall(nth1(I, Ss, [M1|Si])
	       ,(length([M1|Si], L)
		,format('Model set ~w:~n ~w x ~w~n',[I, L, M1])
		,model_value(M1, wargear, Ws)
		,forall(member(Wg-Wn, Ws)
		       ,(model_set_attacks([M1|Si], Wg, D, Mv, Mn, Pa, Wa, _Mod)
			,number_of_attacks(Mn, Pa, Wa, Wn, A)
			,hit_roll(A, (5+), 0, Hn)
			,format('Hits from ~w attacks with weapon ~w: ~w~n', [A,Wg,Hn])
			)
		       )
		)
	       ).



%!	models_attacks(+Unit,+Distance,+Movement,+Attacks) is det.
%
%	Calculate the attacks possible by the models in a Unit.
%
%	Distance is the distance to the target; Movement is the unit's
%	movement, as accepted by model_set_attacks/8 (one of none,
%	standard, advance).
%
%	Attacks is a list of lists of compounds Model-Wargear:Attacks,
%	where Model is the name of the models in a model-set in the
%	Unit, Wargear is a wargear used by Model and Attacks the number
%	of attacks calculated for that model and wargear in the entire
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
