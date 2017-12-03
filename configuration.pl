:-module(configuration, [format_string/2
			,model_characteristics/11
			,print_unit_ability_text/1
			,weapon_characteristics/8
			,unit_ability_text/2
			,weapon_ability_text/3
			,wound_allocation_strategy/1
			]).


%!	format_string(?Predicate,?Format) is det.
%
%	Pretty-printing format used in a display module Predicate.
%
format_string(print_header, '~w ~*+~w ~*+~w ~*+~w~n').

%!	model_characteristics(?Unit,?Name,?M,?WS,?BS,?S,?T,?W,?A,?Ld,?Sv,?Wg)
%	is semidet.
%
%	Names of the fields in a model/12 term listing a model's
%	characteristics, including profile stats, wargear options taken
%	for the model and current number of wounds.
%
%	Used to retrieve the named fields from a model/12 term.
%
model_characteristics(name,'M','WS','BS','S','T','W','A','Ld','Sv',wargear).
%model_characteristics(name,'M','WS','BS','S','T','W','A','Ld','Sv').
% Would be nice to have the model's unit name also.
%model_characteristics(unit,name,'M','WS','BS','S','T','W','A','Ld','Sv',wargear).

%!	print_unit_ability_text(?Bool) is semidet.
%
%	Whether to print short descriptions for units' abilities.
%
%	Because it's a bit cumbersome to come up with short, succinct
%	descriptions of units' abilities that fit nicely into ascii
%	tables, we may want to not even try.
%
print_unit_ability_text(false).

%!	unit_ability_text(?Ability,?Description) is semidet.
%
%	A unit's Ability and a short textual Description of it.
%
%	Unit abilities stored in datasheets module, under abilities/2,
%	only list a keyword, which should eventually be matched to an
%	engine predicate to simulate the use of that ability. For
%	display, it would be nice to have some text, although it can't
%	be too long, lest it overflows table boundaries.
%
%	I don't know how long "long" is for the time being. See module
%	display for the width of the ASCII table used to print out unit
%	datasheets and figure it out from there.
%
unit_ability_text(disgustingly_Resilient, 'Blerch.').
unit_ability_text(nurgles_Gift, 'Aw, look at that plague-riddled stocking!').
unit_ability_text(cataphractii_Armour, 'Cataphractii armour.').
unit_ability_text(teleport_Strike, 'Teleport strike').
unit_ability_text(explodes, 'Boom!').
unit_ability_text(stabilising_anchors, 'Deploy!').
unit_ability_text(walking_battleship, 'Walking. But still, a battleship.').
unit_ability_text(for_the_greater_good, 'For the Greater Good.').
unit_ability_text(bonding_knife_ritual, 'Automatically pass morale tests on a 6.').
unit_ability_text(drone_support, 'Can be accompanied by Tactical Drones.').
unit_ability_text(saviour_protocols, 'You may choose to allocate wounds to nearby drones.').
unit_ability_text(guardian_field, 'Guardian Drone: 5+ Inv. Save; Strike Teams: +6 Inv. Sv').
unit_ability_text(ds8_tactical_support_turret, 'A deploy-once immobile gun turret.').
unit_ability_text(threat_identification_protocols,'Only shoots at nearest visible target.').
unit_ability_text(shield_generator, 'Shild Drones have a 4+ Invulnerable save').
unit_ability_text(stable_platform, 'Ignores heavy weapons shooting penalties.').
unit_ability_text(and_the_shall_know_no_fear, 'They say "no-no" to fear').
unit_ability_text(combat_squads, '10-model squads may be deployed as two 5-model ones').



%!	weapon_characteristics(?Id,?Profile,?Range,?Type,?S,?AP,?D,?Abilities)
%!	is semidet.
%
%	Names of the fields in a weapon/8 term in datasheets module
%	listing a weapon's characteristics.
%
%	Used to retrieve the named fields from weapon/8 terms.
%
weapon_characteristics('Id','Profile','Range','Type','S','AP','D','Abilities').

%!	weapon_ability_text(?Weapon_ability, ?Text) is det.
%
%	A short textual descriptor for one a weapon ability.
%
%	This may cause a bit of abstraction angst, since it's hard to
%	figure out where and how it is used. The first argument is a
%	member of the list of weapon abilities from the last argument
%	of a weapon/8 clause. The second argument is a short textual
%	description of this ability used for display.
%
%	The usual way to get to the text in the second argument of this
%	predicate is to select a weapon/8 row, walk over a list of its
%	abilities (i.e. its last argument) and for each find a clause of
%	this predicate.
%
weapon_ability_text(plague_weapon, 'Plague Weapon',[]).
weapon_ability_text(non_visible_targets, 'May hit non-visible targets',[]).
weapon_ability_text(inflicts_mortal_wounds(N), 'Inflicts ~w mortal wounds',[N]).
weapon_ability_text(single_use,'Single-use',[]).
weapon_ability_text(hit_on_a(N),'Must roll ~w to hit', [N]).
weapon_ability_text(auto_hits,'Hits automatically', []).
weapon_ability_text(if_target(models(M),heavy(N)),'heavy(~w) against ~w+ models',[M,N]).
weapon_ability_text(strips_bonus(B),'Ignores ~w',[B]).
weapon_ability_text(markerlight,'Markerlight',[]).
weapon_ability_text(photon_grenade,'Photon Grenade',[]).


%!	wound_allocation_strategy(?Strategy) is det.
%
%	A Strategy for allocating wounding hits to a unit's models.
%
%	Relevant to unit:wound_allocation_order/3, used to select
%	clauses according to a preferred Strategy.
%
wound_allocation_strategy(fewer_wounds_first).
