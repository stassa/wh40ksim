:-module(unit, [models_unit/3
	       ,model_sets/2
	       ,ordered_model_set/3
	       ,model_set_attacks/5
	       ,model_set_attacks/8
	       ,model_set_movement_distance/3
	       ,model_set_move/2
	       ,wound_allocation_order/3
	       ]).

:-use_module(src(model)).
:-use_module(src(datasheets)).
:-use_module(src(wargear)).

/** <module> Units and model-sets.

The predicates in this module provide a de facto definition of a unit as
a set of model-sets, where each model-set is a set of models that share
the same profile and wargear. This definition, which does not exist in
WH40K rules, is meant to facilitate treating diverse models in a unit
correctly during simulations.

For instance, when determining the number of attacks a unit's models can
make, we need to consider both the value of the models' profile Attack
characteristic, as well as the attack value of their weapons and any
modifiers to those characteristics bestowed by wargear. Since models
with the same profile and wargear will make the same number of attacks
(provided they also moved in the same way during the movement phase etc)
and in order to avoid having to decide all of the above for each model
separately, we split the unit in sets of models with the same profile
and wargear, which we call model-sets.

In terms of the Prolog code, a unit is a list-of-lists where each
sub-list holds model/N terms with the same profile and wargear.

Note that most predicates in this module will not check that a list of
models is really a model-set, i.e. that all the models in the list have
the same profile and wargear. This allows some units to be based as just
lists of models, which is also sometimes useful.
*/



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
	findall(model(Nm,M,WS,BS,S,T,W,A,Ld,Sv,Wg_Nums)
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
		%,once(wargear(Nm, Wg, Num))
		,findall(Wg-Num
			,wargear(Nm,Wg,Num)
			,Wg_Nums)
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



%!	ordered_model_set(+Models,+Ascending,+Descending,-Ordered) is
%!	det.
%
%	Order Movels according to the given partial orderings.
%
%	Ascending and Descending should be terms (T1,T2,...,Tn) where
%	each Ti is a profile characteristic of a model in the set, as
%	defined in the configuration option model_characteristics/M.
%
ordered_model_set(Ms, PO, Os):-
	once(current_functor(model_characteristics, A))
	,A \= 0
	,functor(T, model, A)
	,term_variables(T, Vs)
	,order_terms(PO, Vs, PO_)
	,findall(T
		,(order_by(PO_
			  ,(member(T, Ms)
			   )
			  )
		 )
		,Os).


%!	order_terms(+Terms,+Variables,-Sharing) is det.
%
%	Ensure Terms share with Variables.
%
%	Terms is a list of _ground_ desc/1 and asc/1 terms, whose single
%	arguments should be model characteristics, for instance
%	[desc('W'), asc('Sv')].
%
%	Variables is a list of unbound variables equal in length to the
%	arity of the model_characteristics term in the configuration.
%	Each variable in Variables is meant to bind to one argument of a
%	model/N term in subsequent processing, specifically in the
%	second argument of the order_by/2 call in  ordered_model_set/3.
%
%	order_terms/3 will bind to Sharing a list of the same Terms but
%	with their atomic arguments swapped for the appropriate variable
%	in Variables. The "appropriate variable" is the variable at
%	position I in Variables, where I is the index reported by
%	characteristic_index/2 for each Oi, where Oi one of the elements
%	of Terms.
%
%	For example, in the following query, the ground terms desc('W')
%	and asc('Sv') result in the list [desc(W), asc(Sv)] where W and
%	Sv share with the 7th and 10th variable in the list of model
%	characteristic variables :
%	==
%	?- unit:order_terms([desc('W'),asc('Sv')],[Name,M,WS,BS,S,T,W,A,Ld,Sv,Wargear],Os).
%	Os = [desc(W), asc(Sv)].
%	==
%
order_terms(PO, Vs, PO_):-
	order_terms(PO, Vs, [], PO_).

%!	order_terms(+Terms,+Vars,+Acc,-Bind) is det.
%
%	Business end of order_terms/3.
%
order_terms([], _, PO, OP):-
	reverse(PO, OP)
	,!.
order_terms([Oi|PO], Vs, Acc, Bind):-
	Oi =.. [O,Ai]
	,once(characteristic_index(Ai, I))
	,nth1(I, Vs, Vi)
	,O_ =.. [O,Vi]
	,order_terms(PO, Vs, [O_|Acc], Bind).



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
	,weapon_value(Wg, 'Type', Type)
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
%	@bug This should also take into account Overwatch rules. When a
%	unit is charged, it gets an attack, no?
%
model_set_attacks([M1|Ms], Wg, D, M, Mn, Pa, Wa, P):-
	length([M1|Ms], Mn)
	,model_value(M1, 'A', Pa)
	,weapon_value(Wg, 'Type', Type)
	,weapon_value(Wg, 'Range', R)
	,Type =.. [T,N]
	,(   D =< R
	     % Units can't shoot at enemies less than 1" away.
	     % except with pistols.
	    ,D >= 1
	 ->  weapon_attacks(T, N, D, R, M, Wa, P)
	 ;   D =< R
	    ,weapon = pistol
	 ->  weapon_attacks(T, N, D, R, M, Wa, P)
	 ;   Wa = 0
	    ,P = 0
	 ).


model_set_attacks_([M1|Ms], D, M, Mn, Wn, Pa, Wa, P):-
	length([M1|Ms], Mn)
	,model_value(M1, 'A', Pa)
	,model_value(M1, wargear, Wg-Wn)
	,weapon_value(Wg, 'Type', Type)
	,weapon_value(Wg, 'Range', R)
	,Type =.. [T,N]
	,(   D =< R
	     % Units can't shoot at enemies less than 1" away.
	     % except with pistols.
	    ,D >= 1
	 ->  weapon_attacks(T, N, D, R, M, Wa, P)
	 ;   D =< R
	    ,weapon = pistol
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
%	@bug This doesn't take fall_back into account.
%
weapon_attacks(assault, N, _D, _R, M, A, P):-
	!
	,(  memberchk(M,[none, standard])
	->  A = N
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
	 ->  A = N
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
	 ->  attack_multiplier(N, 2, A)
	 ;   memberchk(M, [none,standard])
	 ->  A = N
	 ;   A = 0
	 )
	,P = 0.

weapon_attacks(grenade, N, _, _, M, A, P):-
	!
	,(   M = advance
	 ->  A = 0
	    ,P = 0
	 ;   A = N
	    ,P = 0
	 ).

weapon_attacks(pistol, N, _, _, M, A, P):-
	!
	,(   M = advance
	 ->  A = 0
	    ,P = 0
	 ;   A = N
	    ,P = 0
	 ).


%!	attack_multiplier(+Attack,+Multiplier,-Result) is det.
%
%	Multiply an attack by a Multiplier.
%
%	Attack may be a number or a compound NdM die size term. If
%	Attack is a number, Result is its product to Multiplier. If
%	Attack is a die size,
%
attack_multiplier(N, M, P):-
	number(N)
	,!
	,P is N * M.
attack_multiplier(NdM, K, KNdM):-
	die_size(NdM, N, _M)
	,KN is N * K
	,die_size(KNdM, KN, 6).


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



%!	model_set_movement_distance(+Models,+Type,-Distance) is det.
%
%	Determine the Distance a model set can move.
%
model_set_movement_distance(_, none, 0):-
	!.
model_set_movement_distance(Ms, T, D):-
	(   T = standard
	;   T = fall_back
	)
	,!
	,model_set_move(Ms, D).
model_set_movement_distance(M, advance, D):-
	!
	,model_set_move(M, V)
	,roll('1d6', A)
	,D is V + A.
model_set_movement_distance(_, charge, D):-
	roll('2d6', D).



%!	model_set_move(+Models, -Rate) is det.
%
%	Determine the lowest Movement stat in Models.
%
%	Use this to decide how far a set of models can move in a
%	movement phase without breaking unit coherency.
%
model_set_move(Ms, Mr):-
	ordered_model_set(Ms, [asc('M')], [M|_])
	,model_value(M, 'M', Mr).



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
