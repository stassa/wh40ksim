:-module(unit, [model_sets/2
	       ,model_set_attacks/4
	       ,models_unit/3]).

:-use_module(src(model)).
:-use_module(src(datasheets)).


%!      models_unit(+Choices, +Unit_name, -Unit) is det.
%
%	Compose a Unit from the given model Choices.
%
%	Unit_name is an atomic name used as an identifier for the unit.
%	Unit is bound to a key-value pair Unit_name-Ms, where Ms the
%	models in the unit.
%
%	For now, Choices is expected to be a list of key-value pairs
%	M-N where M is the name of a model from datasheets module and N
%	the number of that model to include in the Unit.
%
%	More Choices will have to be allowed to be made, such as wargear
%	options and, er, wargear options. Additionally, unit composition
%	options must be checked, so that, for example, we do not
%	allow a unit to be composed of 5 Fire Warrior Shashu'i and 3
%	Space Marines.
%
%	Ms, the list of models in the unit, is bound to a list of terms
%	model/N holding model information. Eventually this should
%	include wargear etc options. For the time being, the only
%	information is the model's profile.
%
models_unit(Cs, Un, Un-Us):-
	findall(model(Nm,M,WS,BS,S,T,W,A,Ld,Sv,Wg)
	       ,(member(Nm-N, Cs)
		,between(1, N, _)
		,unit_profiles(_Id,Nm,M,WS,BS,S,T,W,A,Ld,Sv)
		% This will only get the first wargear option
		% for the unit, so it's completely wrong
		% but it will do for now.
		% Wg should probably expand the wargear of
		% the model, including each item once for its Num
		% i.e. the number of times that item is equipped
		% on the model.
		,once(wargear(Nm, Wg, _Num))
		)
	       ,Us).



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


