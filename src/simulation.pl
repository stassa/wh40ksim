:-module(simulation, [shooting_sequence/3
		     ,number_of_attacks/5
		     ,hit_roll/3
		     ,wound_roll/4
		     ,allocate_wounds/3
		     ,saving_throws/3
		     ,inflict_damage/3
		     ]).

:-use_module(src(datasheets)).
:-use_module(src(unit)).
:-use_module(src(model)).
:-use_module(src(wargear)).

/** <module> Predicates to simulate combat between sets of models.

*/


%!	shooting_sequence(+Attacker, +Target, -Surviving) is det.
%
%	Simulate a round of shooting against a Target.
%
%	Attacker and Target must both be units, given as lists of
%	model/N terms (see configuration:model_characteristics/N).
%
%	Surviving is a list of the models remaining in Target after one
%	round of shooting by the models in Attacker.
%
%	Note that less than the entire Shooting Sequence defined by the
%	rules (in pg. 179 in 8th edition rulebook) is simulated.
%	Specifically, the shooter and target selection is implicit in
%	the call to this predicate (since they're passed as arguments)
%	and no attempt is made to implement the target- and shooting-
%	selection logic.
%
%	Limitations of the implementation so-far.
%	=========================================
%
%	1. The current implementation expects an Attacker with a single
%	wargear item. Note that it still correctly takes into account
%	models equipped with multiples thereof; for insance, gun drones
%	equipped with two pulse carbines are allowed to make an attack
%	with each.
%
%	2. It also expects a Target with no wargears, or in any case
%	will not take into account wargear selections that may grant
%	save or Toughness modifiers etc.
%
%	3. Although the implementation will correctly determine attacks
%	by an Attacker with diverse models, it expects a Target with
%	uniform composition (i.e. only one type of model).
%
%	4. Models with * in their profiles are not treated correctly.
%
%	Further Notes
%	=============
%
%	To be honest, I'm not sure if that third bullet point above is
%	actually a limitation. It's possible that the rules never allow
%	for models with diverse Toughness to be organised in a unit (for
%	instance, Fire Warriors with T3 may be accompanied by Tactical
%	Drones with T4, but the drones don't actually form part of the
%	unit as such- they're deployed together but are treated
%	separately). The "fast die rolling" sidebar on page 179 of the
%	8th Ed. rulebook make it sound like you can batch-roll to wound
%	against a single unit, without any mention to what happens if
%	models in the unit have different Toughness. It may just be that
%	they 're not meant to.
%
%	Btw, the Fast Die Rolling sidebar states that in order to make
%	several attacks at once, all the attacks must have the same BS,
%	S, AP and D. I think this is covered already by the model-set
%	selection logic that looks for models with the same profile and
%	wargear -which will therefore have the same BS, S, AP and D.
%
shooting_sequence(As, Ts, Ss):-
	model_sets(As, Ms)
	,shooting_sequence_(Ms, Ts, Ss).

shooting_sequence_([], Ss, Ss):-
	!.
shooting_sequence_([Mi|Ms], Ts, Bind):-
	Mi = [M1|_]
	,model_value(M1, 'BS', BS)
	,model_value(M1, 'wargear', Wg-_Num)
	,weapon_value(Wg, 'S', S)
	,weapon_value(Wg, 'AP', AP)
	,weapon_value(Wg, 'D', D)
	,Ts = [T1|_]
	,model_value(T1, 'T', T)
	,model_set_attacks(Mi, Mn, Wn, Pa, Wa)
	,number_of_attacks(Mn, Pa, Wa, Wn, As)
	,hit_roll(As, BS, Hn)
	,wound_roll(Hn, S, T, Ws)
	,allocate_wounds(Ws, Ts, Ms_Ws)
	,saving_throws(Ms_Ws, AP, Fs)
	,inflict_damage(Fs, D, Rs)
	,remove_casualties(Rs, Ss)
	,shooting_sequence_(Ms, Ss, Bind).



%!	number_of_attacks(+M, +Pa, +Wa, +Wn, -A) is det.
%
%	Calculate the number of attacks for a set of models.
%
%	M is the number of models in the model-set, each of which share
%	a profile number of attacks and carry the same number of the
%	same weapons.
%
%	Pa is then the number of attacks listed in the model-set's model
%	profile, Wa is the number of attacks for their equipped weapon
%	and Wn the numbers of that weapon equipped.
%
%	A is calculated as the product M * Pa * Wa * Wn, the total
%	number of attacks all the models in the unit can attempt.
%
number_of_attacks(M, Pa, Wa, Wn, N):-
	N is M * Pa * Wa * Wn.



%!	hit_roll(+A, +BS, -Hn).
%
%	Roll to hit with an attack from a set of models.
%
%	A is the number of attacks the model-set is making, as
%	calculated by number_of_attacks/5. BS is the Ballistic Skill of
%	the models in the model-set.
%
%	Hn is then the number of successful hits, i.e. rolls on 1d6 that
%	are equal to, or higher than the model-set's BS.
%
%	@tbd Remember to wrap BS in ()'s, as in (5+), in the top-level
%	as well as source code to avoid operator clashes.
%
hit_roll(A, BS, Hn):-
	roll_vs_tn(A, '1d6', BS, Hn).



%!	wound_roll(+Hn,+S,+T,-Wn) is det.
%
%	Roll to wound for a number of successful hits.
%
%	Hn is the number of a model-set's attacks that hit successfully,
%	as calculated in hit_roll/3. S is the Strength characteristic of
%	models in the model-set; T is the thoughness characterisic of
%	models in the target unit.
%
%	 Wn is then the number of hits that successfully wound,
%	 calculated as the number of rolls on 1d6 that are equal to, or
%	 higher than the target number determined by the relative values
%	 of S and T. See to_wound/3 for details of the target number
%	 determination.
%
wound_roll(Hn, S, T, Wn):-
	to_wound(S, T, Tn)
	,roll_vs_tn(Hn, '1d6', Tn, Wn).


%!	to_wound(+S,+T,-Tn) is det.
%
%	Determine the target number required to wound.
%
%	S is the Strength characteristic of an attacking unit; T is the
%	Toughness of the target unit; Tn is the target number when
%	rolling to wound the target unit with attacks by the attacking
%	unit.
%
%	See page 181 of the 8th edition rulebook, box Wound Roll, for
%	more information.
%
to_wound(S,T,2+):-
	T_ is T * 2
	,S >= T_
	,!.
to_wound(S, T, 3+):-
	S > T
	,!.
to_wound(S, S, 4+):-
	!.
to_wound(S, T, 5+):-
	S < T
	,!.
to_wound(S, T, 6+):-
	S_ is S * 2
	,S_ =< T.



%!	allocate_wounds(+Hn,+Us,-Ms) is det.
%
%	Allocate successfully wounding hits to a unit's models.
%
%	Hn is the number of an attacker's hits that successfully wound
%	models in a target unit U, as calculated by wound_roll/4. Us is
%	the target unit, U, with models sorted according to the wound
%	allocation strategy defined in the configuration.
%
%	Ms is then a list of key-value pairs, Mi-Wi, where each Mi
%	is a model in Us to which one or more wounds are allocated and
%	Wi the number of wounds allocated to that model.
%
allocate_wounds(Hn, Us, Ms):-
	configuration:wound_allocation_strategy(S)
	,wound_allocation_order(S, Us, Us_)
	,! % cut off search for more S
	,allocate_wounds(Us_, Hn, [], Ms).


%!	allocate_wounds(+Models,+Wounds,+Acc,-Bind) is det.
%
%	Business end of allocate_wounds/3.
%
allocate_wounds(_, 0, Ms, Ms):-
	!.
allocate_wounds([], Hn, Acc, Bind):-
	!
	,allocate_wounds(Acc, Hn, [], Bind).
allocate_wounds([Mi-W|Ms], Hn, Acc, Bind):-
	!
	,Hn_ is Hn - 1
	,succ(W, Wi)
	,allocate_wounds(Ms, Hn_, [Mi-Wi|Acc], Bind).
allocate_wounds([Mi|Ms], Hn, Acc, Bind):-
	Hn_ is Hn - 1
	,allocate_wounds(Ms, Hn_, [Mi-1|Acc], Bind).



%!	saving_throws(+Ms, +AP, -Fs) is det.
%
%	Roll saving throws for models in a unit under attack.
%
%	Ms is a list of key-value pairs Mi-Wi, where each Mi is a model
%	and Wi the number of wounds allocated to this model in the
%	Allocate Wounds step, as calculated by allocate_wounds/3, and Wi
%	the number of wounds allocated to that model. AP is the Armour
%	Penetration characteristic of the attacking model-set.
%
%	Fs is then a list of key-value pairs, Mi-Fi, where each Mi is a
%	model in Ms and each Fi is the number of saved failing throws
%	rolled by that unit against the wounds allocated to it, i.e. the
%	number of wounds remaining unsaved against this model.
%
saving_throws(Ms, AP, Fs):-
	findall(Mi-F
	       ,(member(Mi-Ws, Ms)
		,once(model_value(Mi,'Sv', Sv))
		,roll_vs_tn_mod(Ws, '1d6', Sv, AP, R)
		,F is Ws - R
		,F > 0
		)
	       ,Fs).



%!	inflict_damage(+Ms, +D, -Rs) is det.
%
%	Calculate the damage taken by each model in a unit.
%
%	Ms is a set of key-value-pairs, Mi-Wi, where each Mi is a model
%	that failed at least one save against a wound it was allocated,
%	as calculatd by saving_throws/3 and each Wi is the number of
%	such failed saves.
%
%	Rs is the list of models in Ms with their remaining wounds W,
%	reduced by D, times the number of unsaved wounds allocated to
%	each, Wi. In other words, for each unsaved wound, a model takes
%	full damage from an attack. And then takes some more. No mercy,
%	Percy.
%
%	Note that Rs is not a key-value pair anymore: the number of
%	wounds in each model/N term in Ms is updated destructively.
%
%	@tbd Just for fun (and information) I could also add to the
%	output the amount of damage the dying model took, binding a list
%	of key-value pairs in the output.
%
inflict_damage(Ms, D, Rs):-
	findall(Mi
	       ,(member(Mi-Fi, Ms)
		,model_value(Mi, 'W', W)
		,W_ is W - Fi * D
		,model_value(Mi, 'W', W_)
		)
	       ,Rs).



%!	remove_casualties(+Models, -Surviving) is det.
%
%	Remove casualties from a set of Models.
%
remove_casualties(Ms, Ss):-
	findall(Mi
	       ,(member(Mi, Ms)
		,model_value(Mi, 'W', Wi)
		,Wi > 0
		)
	       ,Ss).

