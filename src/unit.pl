:-module(unit, [model_sets/2
	       ,model_set_attacks/5
	       ,model_set_attacks/8
	       ,models_unit/3
	       ,wound_allocation_order/3]).

:-use_module(src(model)).
:-use_module(src(datasheets)).
:-use_module(src(wargear)).

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
	findall(model(Nm,M,WS,BS,S,T,W,A,Ld,Sv,Wg-Num)
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
		,once(wargear(Nm, Wg, Num))
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



%!	model_set_attacks(+Model_set,-Mn,-Wn,-Pa,-Wa) is det.
%
%	Determine profile and weapon attack numbers for a Model_set.
%
%	Mn is the number of models in the Model_set; Wn is the numbers
%	of a single weapon equipped by models in the set; Pa is the
%	'A' value in the models' profile (their "profile attacks") and
%	Wa the number of attacks on the profile of that weapon (its
%	"weapon attacks").
%
%	This predicate is meant as a helper to get the required
%	information for simulation:number_of_attacks/5. The name is a
%	bit off- we're not actually calculating the attacks the model
%	set can do, just getting the information required to do that.
%	But it's a long name already.
%

model_set_attacks([M1|Ms], Mn, Wn, Pa, Wa):-
	length([M1|Ms], Mn)
	,model_value(M1, 'A', Pa)
	,model_value(M1, wargear, Wg-Wn)
	,weapon(Wg,_,_,Type,_,_,_,_)
	,weapon_attacks(Type, Wa).



%!	model_set_attacks(+Model_set,+D,+M,-Mn,-Wn,-Pa,-Wa,-P) is det.
%
%	As model_set_attacks/5 but tracks distance, movement etc.
%
%	D is the distance of the shooting unit to the target (rapid fire
%	weapons double their attacks when in half-range to their
%	target). M is the type of the unit's last movement (some weapon
%	types apply penalties for some types of movement and most types
%	of weapons can't be shot when advancing). P is the penalty
%	applied by the weapon to the to-hit roll of the bearer (heavy
%	and assault weapons bestow a -1 penalty if the bearer moved or
%	advanced, respectively, last turn).
%
%	@tbd This will always bind Pa to the number of attacks in the
%	unit's profile, regardless of any battlefield conditions. That
%	number will end up multiplied by weapon attacks anyway, so if
%	those are 0, the final number of attacks the model will be able
%	to make will be 0.
%
model_set_attacks([M1|Ms], D, M, Mn, Wn, Pa, Wa, P):-
	length([M1|Ms], Mn)
	,model_value(M1, 'A', Pa)
	,model_value(M1, wargear, Wg-Wn)
	,weapon_value(Wg, 'Type', Type)
	,weapon_value(Wg, 'Range', R)
	,Type =.. [T,N]
	,(   D =< R
	    ,D > 0 % otherwise, invalid
	 ->  weapon_attacks(T, N, D, R, M, Wa, P)
	 ;   Wa = 0
	    ,P = 0
	 ).



%!	weapon_attacks(+Type, -Attacks) is det.
%
%	Determine the number of Attacks a Weapon can make.
%
%	Naive version that doesn't care about anything but what's inside
%	a weapon's type term (e.g. assault(1), heavy(d3) etc).
%
weapon_attacks(T, A):-
	T =.. [_Type,A_]
	% datasheets module defines random attack numbers
	% as d-N, which is a bit off with the way
	% die sizes are defined. Needs fixin'.
	,(   A_ = d-N
	 ->  roll(1, N, A)
	 ;   A_ = A
	 ).


%!	weapon_attacks(+Type,+N,+D,+R,+M,-A,-P) is det.
%
%	As weapon_attacks/2 but also tracks Movement, Distance etc.
%
%	N is the number in the weapon's type, e.g. assault(1) or
%	heavy(d6) etc. D is the distance to the target. R is the
%	weapon's maximum range. M is the type of movement (one of:
%	[none,standard, advance, flight(?)]).
%
%	A is the number of attacks possible with that weapon and P a
%	penalty to hit rolls applied by some weapon types and movement
%	combinations, e.g. when a unit armed with a heavy weapon moves
%	it takes a -1 penalty to its rolls to hit, etc.
%
weapon_attacks(assault, N, _D, _R, M, A, P):-
	!
	,(   memberchk(M,[none, standard])
	->  type_attacks(N, A)
	   ,P = 0
	;   M = advance
	->  type_attacks(N, A)
	   ,P = -1
	;   A = 0
	   ,P = 0
	).

weapon_attacks(heavy, N, _D, _R, M, A, P):-
	!
	,(   M = standard
	 ->  type_attacks(N, A)
	    ,P = -1
	;    M = none
	 ->  type_attacks(N, A)
	    ,P = 0
	;    A = 0
	    ,P = 0
	).

weapon_attacks(rapid_fire, N, D, R, M, A, P):-
	!
	,R_ is round(R / 2)
	,(   memberchk(M, [none,standard])
	    ,D =< R_
	 ->  type_attacks(N, A_)
	    ,A is A_ * 2
	 ;   memberchk(M, [none,standard])
	 ->  type_attacks(N, A)
	 ;   A = 0
	 )
	,P = 0.

weapon_attacks(grenade, _, _, _, M, A, P):-
	!
	,(   M = advance
	 ->  A = 0
	    ,P = 0
	 ;   A = 1
	    ,P = 0
	 ).

weapon_attacks(pistol, _, _, _, M, A, P):-
	!
	,(   M = advance
	 ->  A = 0
	    ,P = 0
	 ;   A = 1
	    ,P = 0
	 ).


%!	type_attacks(+Number, -Attacks) is det.
%
%	Determine number of attacks from weapon type value.
%
%	"Weapon type value" refers to the term encapsulated in weapon
%	types, which is normally either a number or a die size, as for
%	instance in heavy(D6) or assault(1) etc.
%
%	Number should be such a Weapon type value; Attacks is either the
%	number in the WTV, or the result of a die roll with that die
%	size.
%
%	@bug We shouldn't be rolling the dice for an attack now-
%	instead, leave that up to the simulation or rollout. We'll need
%	to recalculate in each round or rollout.
%
%
type_attacks(N, A):-
	(   N = d-M
	 ->  roll(1, M, A)
	 ;   number(N)
	    ,A = N
	 ).


%!	wound_allocation_order(+Strategy, +Models, -Ordered) is det.
%
%	Determine wound allocation order for to the given Strategy.
%
%	Strategy is an atom selecting for different wound allocation
%	orderings of the models in list Models.
%
%	Each Strategy determines the order in which successfully
%	wounding hits are allocated to the models in Models.
%
%	Each i'th successfully wounding hit Wi should then be allocated
%	to the i'th model in the re-Ordered Models list, Mi: the first
%	wounding hit W1 is allocated to the first model in the Ordered
%	list, M1, then the second wound to the second model and so on,
%	until wounds or models run out.
%
%	This predicate only selects the wound allocation strategy and
%	re-orders the models in a unit. Actual wound allocation is
%	handled by simulation:allocate_wounds/3, according to the
%	Strategy defined in configuration module.
%
%	Currently, known Strategy options are:
%	* fewer_wounds_first: Allocates wounds to models with fewer
%	remaining wounds first, then to the rest of the models without
%	any specific order.
%
wound_allocation_order(fewer_wounds_first, Ms, Ms_):-
	characteristic_index('W', I)
	,sort(I, @>=, Ms, Ms_).



