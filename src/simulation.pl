:-module(simulation, [model_sets/2
		     ,model_set_attacks/4
		     ,number_of_attacks/4]).

:-use_module(src(model)).
:-use_module(src(datasheets)).


%!	model_sets(+Unit, -Model_sets) is det.
%
%	Partition a Unit into sets of Models according to their stats.
%
%	Model_sets is a list of lists of models with the same number of
%	Attacks listed on their profile and with the same equipped
%	weapons (therefore, number of attacks listed on their weapons
%	profiles).
%
%	@tbd This will certainly break once I change the definition of
%	models_unit/3 to allow for different wargear choices for models
%	in a unit.
%
model_sets(Us, Ss):-
	findall(Gi
	       ,(group_by((A,Wg)
			,M
			,(member(M, Us)
			 ,model_value(M, 'A', A)
			 ,model_value(M, wargear, Wg)
			 )
			,Gi)
		)
	       ,Ss).


%!	model_set_attacks(+Model_set,+Models,+Profile_attacks,+Weapon_attacks)
%!	is det
%
%	Determine profile and weapon attack numbers for a Model_set.
%
model_set_attacks([M1|Ms], M, Pa, Wa):-
	length([M1|Ms], M)
	,model_value(M1, 'A', Pa)
	,model_value(M1, wargear, Wg)
	,weapon(Wg,_,_,Type,_,_,_,_)
	,weapon_attacks(Type, Wa).


%!	weapon_attacks(+Type, -Attacks) is det.
%
%	Determine the number of Attacks a Weapon can make.
%
weapon_attacks(T, A):-
	T =.. [_Type,A_]
	% Datasheets define random attack numbers
	% as d-N, which is a bit off with the way
	% die sizes are defined. Needs fixin'.
	,(   A_ = d-N
	 ->  roll(1, N, A)
	 ;   A_ = A
	 ).


%!	number_of_attacks(+M, +Pa, Wa, -N) is det.
%
%	Calculate the number of attacks for a set of models.
%
%	M is the number of models in the model-set, each of which share
%	a profile number of attacks and an equipped weapon type.
%
%	Pa is then the number of attacks listed in the model-set's model
%	profile and Wa is the number of attacks for their equipped
%	weapon type.
%
%	N is the produce M * Pa * Wa, the total number of attacks all
%	the models in the unit can attempt.
%
number_of_attacks(M, Pa, Wa, N):-
	N is M * Pa * Wa.
