:-module(datasheets, [op(200,yf,+)
		     ,op(700,fy,<)
		     ,op(200,yf,>)
		     ,unit/3
		     ,battlefield_role/2
		     ,power/2
		     ,unit_profiles/11
		     ,damaged_profiles/4
		     ,unit_composition/3
		     ,unit_composition_options/6
		     ,wargear/3
		     ,wargear_options/5
		     ,abilities/2
		     ,weapons/2
		     ,weapon/8
		     ,faction_keyword/2
		     ,keyword/2
		     ]).

:-op(200,yf,+).
:-op(700,fy,<).
:-op(200,yf,>).

%!	unit(?Id, ?Name, ?Faction) is semidet.
%
%	A unit's Id, its Name and Faction.
%
%	A unit's Id should be used to identify the unit throughout
%	the system, while its Name is for display. A unit's Id will
%	normally be its name, lowercased and with underscores replacing
%	spaces.
%
%	It's funny but Wh40k datasheets don't mention faction, as such:
%	the information of a unit's faction affiliation is in the unit's
%	keyword list. Therefore, listing the Faction in this predicate
%	is not, strictly speaking, necessary. On the other hand, it's
%	very convenient to have it listed here; the alternative is to
%	search each unit's list of keywords for a faction name- which is
%	a little cumbersome.
%
unit(lord_of_contagion, 'Lord of Contagion', chaos).
unit(kv128_stormsurge, 'KV128 Stormsurge', tau_empire).
unit(strike_team, 'Strike Team', tau_empire).
unit(tactical_drones, 'Tactical Drones', tau_empire).
unit(tactical_squad, 'Tactical Squad', adeptus_astartes).


%!	battlefield_role(?ID, ?Role) is semidet.
%
%	A unit's battlefield role.
%
%	Used when making a Battle-forged army.
%
%	Symbols used on datasheets:
%	* Skull: HQ
%	* Play: Troops
%	* Skullcross: Elites
%	* Thunder: Fast Attack
%	* Explosion: Heavy Support
%	* Skullarrow: Dedicated Transport
%	* Wingsword: Flyer
%	* Rook: Fortification
%	* Gauntlet: Lord of War
%
%	Also see page 241 on the 8th edition rulebook.
%
battlefield_role(lord_of_contagion, hq).
battlefield_role(kv128_stormsurge, lord_of_war).
battlefield_role(strike_team,troops).
battlefield_role(tactical_drones,fast_attack).
battlefield_role(tactical_squad,troops).



%!	power(?ID, ?Power) is semidet.
%
%	A unit's Power level.
%
power(lord_of_contagion, 9).
power(kv128_stormsurge, 22).
power(strike_team,3).
power(tactical_drones,2).
power(tactical_squad,5).



%!	unit_profiles(?Id,?Name,?M,?WS,?BS,?S,?T,?W,?A,?Ld,?Sv) is
%!	semidet.
%
%	A unit's profiles - the characteristics of the unit's members.
%
%	For units with multiple types of models, the profile of each
%	type of model is listed under a clause with the unit's ID and
%	the model's Name. For units with a single type of model, Name is
%	the same as their Id.
%
%	When adding units to the database, use the following template to
%	quickly and accurately fill in unit details:
%	==
%	unit_profiles(Id,Name,M,WS,BS,S,T,W,A,Ld,Sv)
%	==
%
unit_profiles(lord_of_contagion,lord_of_contagion,4,2+,2+,4,5,6,4,9,2+).
unit_profiles(kv128_stormsurge,kv128_stormsurge,6,5+,*,*,7,20,*,8,3+).
unit_profiles(strike_team,fire_warrior,6,5+,4+,3,3,1,1,6,4+).
unit_profiles(strike_team,fire_warrior_shasui,6,5+,4+,3,3,1,1,7,4+).
unit_profiles(strike_team,ds8_tactical_support_turret,nil,nil,4+,3,3,1,0,4,4+).
unit_profiles(strike_team,mv36_guardian_drone,8,5+,5+,3,4,1,1,6,4+).
unit_profiles(tactical_drones,mv1_gun_drone,8,5+,5+,3,4,1,1,6,4+).
unit_profiles(tactical_drones,mv4_shield_drone,8,5+,5+,3,4,1,1,6,4+).
unit_profiles(tactical_drones,mv7_marker_drone,8,5+,5+,3,4,1,1,6,4+).
unit_profiles(tactical_squad,space_marine,6,3+,3+,4,4,1,1,7,3+).
unit_profiles(tactical_squad,space_marine_sergeant,6,3+,3+,4,4,1,2,8,3+).



%!	damaged_profiles(?Id,?Max_wounds,?Min_wounds,?Mods) is det.
%
%	Changes to a unit's profile as damage accrues.
%
damaged_profiles(kv128_stormsurge,20,11,[4+,8,3]).
damaged_profiles(kv128_stormsurge,10,6,[5+,7,d3]).
damaged_profiles(kv128_stormsurge,5,1,[6+,6,1]).



%!	unit_composition(?Id, ?Model_type, ?Number) is semidet.
%
%	The number and types of models in a unit.
%
unit_composition(lord_of_contagion,single_model,1).
unit_composition(kv128_stormsurge,single_model,1).
unit_composition(strike_team,fire_warrior,5).
% Veeery bad. A tactical_drones unit can include 4 "tactical drones"
% each of which can be one of three types of drone. Gaddammit.
unit_composition(tactical_drones,tactical_drone,4).
unit_composition(tactical_squad,space_marine_sergeant,1).
unit_composition(tactical_squad,space_marine,4).



%!	unit_composition_options(?Id,?Model,?Number_in,?Substitute,?Number_out,?Power)
%!	is semidet.
%
%	Optional changes to a unit's composition.
%
%	Each option lists the type and number of Model that can be added
%	to the unit, the type and number of model it Substitutes for and
%	the change to the Power level of the unit if that option is
%	taken.
%
%	When additional models of an already existing type may be added
%	to a unit, the atom "nil" should be used in place of a
%	Substitute name and numbers.
%
%	Some options may include multiple models in the same "slot" (as
%	a single addition or substitution). In that case, the name and
%	number of such models is given as a list.
%
%	@tbd In some cases, a model is listed in the unit's profiles but
%	instead of it being given as part of the unit's composition it's
%	instead listed as a wargear option. This is the case with DS8
%	Tactical Support Turrets that can be selected as a warger option
%	by T'Au Fire Warrior Strike Teams. Since such "wargear" have a
%	separate model and profile, we will consider them as unit
%	composition options, rather than wargear options. In the case of
%	DS8 Tactical Drones, they also have two weapon options. These
%	should be treated as wargear options for that type of model.
%	This is probably a bit of a "bug" in the datasheet organisation.
%
%	@tbd A better notation for adding/ substituting unit may be
%	along the lines of:
%	==
%	...,fire_warrior_shashui,+1,fire_warrior,-1,...
%	==
%	Where it's obvious that you're adding one Fire Warrior Shasu'ui
%	and subtracting one Fire Warrior. Then, if a model is being
%	added without replacing another, we can just write:
%	==
%	...,fire_warrior,+1,fire_warrior,0,...
%	==
%
%	The more complicated cases with multiple models in a single slot
%	will still suck.
%
%	@tbd Some options represent models chosen from different units
%	with their own unit_profiles. For instance, this is the case
%	with T'au Tactical Drones that can accompany Strike Teams (and
%	other units). These will have to be represented with a compound
%	unit(N) where N the name of the model that can be selected as a
%	unit composition option. The rules for including such models in
%	a unit should be handled separately- they can't very well be
%	represented as unit clauses, innit.
%
unit_composition_options(lord_of_contagion, nil, nil, nil, nil, nil).
unit_composition_options(kv128_stormsurge, nil, nil, nil, nil, nil).
unit_composition_options(strike_team,fire_warrior,+5,nil,nil,+2).
unit_composition_options(strike_team,fire_warrior,+7,nil,nil,+3).
unit_composition_options(strike_team,fire_warrior_shasui,1,fire_warrior,1,0).
unit_composition_options(strike_team,unit(tactical_drone),2,nil,nil,+1).
unit_composition_options(strike_team,[unit(tactical_drone)
				     ,mv36_guardian_drone],[1,1],nil,nil,+1).
unit_composition_options(strike_team,ds8_tactical_support_turret,+1,nil,nil,0).
unit_composition_options(tactical_drones,tactical_drone,+4,nil,nil,+2).
unit_composition_options(tactical_drones,tactical_drone,+8,nil,nil,+4).
unit_composition_options(tactical_squad,space_marine,+5,nil,nil,+4).



%!	wargear(?Id, ?Wargear, ?Number) is semidet.
%
%	Basic weapons and equipment on unit models.
%
%	Each wargear/3 clause is one Wargear item the unit comes
%	equipped with. Number lists the number of instances of the item
%	on the unit.
%
wargear(lord_of_contagion, plaguereaper, 1).
wargear(kv128_stormsurge, cluster_rocket_system, 1).
wargear(kv128_stormsurge, destroyer_missiles, 4).
wargear(kv128_stormsurge, flamer, 2).
wargear(kv128_stormsurge, pulse_blastcannon, 1).
wargear(kv128_stormsurge, smart_missile_system, 2).
wargear(fire_warrior,pulse_rifle,1).
wargear(fire_warrior,photon_grenade,1).
wargear(fire_warrior_shasui,pulse_rifle,1).
wargear(fire_warrior_shasui,photon_grenade,1).
wargear(ds8_tactical_support_turret,missile_pod,1).
wargear(ds8_tactical_support_turret,smart_missile_system,1).
wargear(mv36_guardian_drone,guardian_field,1).
% Shield and guardian drones don't have wargear but instead
% generate a shield or guardian field. How to reprsent this?
wargear(mv1_gun_drone,pulse_carbine,2).
wargear(mv4_shield_drone,shield_generator,1).
wargear(mv7_marker_drone,markerlight,1).
wargear(space_marine,boltgun,1).
wargear(space_marine,bolt_pistol,1).
wargear(space_marine,frag_grenade,1).
wargear(space_marine,krak_grenade,1).
wargear(space_marine_sergeant,boltgun,1).
wargear(space_marine_sergeant,bolt_pistol,1).
wargear(space_marine_sergeant,frag_grenade,1).
wargear(space_marine_sergeant,krak_grenade,1).


%!	wargear_options(?Id,?Item,+Number_out,+Substitute,+Number_in)
%	is semidet.
%
%	Optional substitutions of a unit's wargear items.
%
%	Each clause lists the wargear Item and is Substitute, along with
%	their respective numbers (where Number_out is the number of
%	instances of the Item to be replaced and Number_in the number of
%	instances of the Substitute to replace them with). Each Item and
%	Substitute are the names of items in the unit's wargear list (as
%	given in wargerar/3).
%
%	When an Item can be selected as an option without replacing
%	anything (such as when choosing items from T'au Support Systems
%	etc) Substitute and Number_in should both be set to the atom
%	"nil".
%
%	In some cases, a single item may be replaced with a number of
%	different options; for instance, a space marine can replace his
%	boltgun from the special weapons list _or_ the heavy weapons
%	list. Such options are listed as separate clauses. That should
%	suffice, given thatit's impossible to replace a single boltgun
%	with two items (or at least we should be able to check this by
%	simple subtraction).
%
wargear_options(kv128_stormsurge,flamer,+2,burst_cannon,-2).
wargear_options(kv128_stormsurge,flamer,+2,airbursting_fragmentation_projector,-2).
wargear_options(kv128_stormsurge,pulse_blastcannon,+1,pulse_driver_cannon,-1).
wargear_options(kv128_stormsurge,support_systems,3,nil,nil).
wargear_options(fire_warrior,pulse_rifle,+1,pulse_carbine,-1).
wargear_options(fire_warrior_shasui,pulse_rifle,+1,pulse_carbine,-1).
wargear_options(fire_warrior_shasui,markerlight,+1,nil,nil).
wargear_options(fire_warrior_shasui,pulse_pistol,+1,nil,nil).
% Not sure about whether the space marine sergeant can replace only
% his bolt pistol or boltgun, or he must replace both with equipment.
wargear_options(space_marine_sergeant,seargeant_equipment,+1,bolt_pistol,-1).
wargear_options(space_marine_sergeant,seargeant_equipment,+1,boltgun,-1).
% This is only true when the squad has fewer than 10 models. Need to be
% able to reprsent conditions like this.
% Also, when the squad has ten models, two marines can take items from
% the heavy and special weapons lists, but only one marine can take an
% item from each list. That should be possible to represent also.
wargear_options(space_marine,special_weapons,+1,boltgun,-1).
wargear_options(space_marine,heavy_weapons,+1,boltgun,-1).


%!	abilities(?Id, ?Abilities) is semidet.
%
%	A unit's special abilities.
%
abilities(lord_of_contagion, disgustingly_Resilient).
abilities(lord_of_contagion, nurgles_Gift).
abilities(lord_of_contagion, cataphractii_Armour).
abilities(lord_of_contagion, teleport_Strike).
abilities(kv128_stormsurge, explodes).
abilities(kv128_stormsurge, stabilising_anchors).
abilities(kv128_stormsurge, walking_battleship).
abilities(strike_team, for_the_greater_good).
abilities(strike_team, bonding_knife_ritual).
abilities(strike_team, drone_support).
abilities(strike_team, saviour_protocols).
abilities(strike_team, guardian_field).
abilities(strike_team, ds8_tactical_support_turret).
abilities(tactical_drones, for_the_greater_good).
abilities(tactical_drones, drone_support).
abilities(tactical_drones, saviour_protocols).
abilities(tactical_drones, threat_identification_protocols).
abilities(tactical_drones, shield_generator).
abilities(tactical_drones, stable_platform).
abilities(tactical_squad,and_the_shall_know_no_fear).
abilities(tactical_squad,combat_squads).



%!	weapons(?Id, ?Weapons) is semidet.
%
%	List of weapons a unit comes equiped with.
%
%	Each member of the list Weapons is an identifier for a clause of
%	weapons/8, storing the weapon's characteristics.
%
%	@tbd this is either an alternative for wargear/2 or redundant.
%	It's hard to tell because the datasheets in the main book and
%	in the Indices are in different format.
%
weapons(lord_of_contagion, plaguereaper-1).



%!	weapon(?Id,?Profile,?Range,?Type,?S,?AP,?D,?Abilities) is
%!	semidet.
%
%	A weapon's characteristics.
%
%	Each member of the list Abilities is an identifier for a clause
%	of weapon_abilities/n, storing the details of that ability. An
%	empty list signifies no special abilities.
%
%	For weapons with multiple profiles, each profile is given as a
%	different clause with the name of the profile given in Profile.
%	Weapons with a single profile have the atom "base" in that
%	position, instead.
%
%	Use the following template to fill in weapon stats quickly:
%	==
%	weapon(Id,Profile,Range,Type,S,AP,D,Abilities)
%	==
%
%
weapon(plaguereaper, base, melee, melee, +2, -3, 3, [plague_weapon]).
weapon(airbursting_fragmentation_projector,base,18,assault(d6),4,0,1,[non_visible_targets]).
weapon(burst_cannon,base,18,assault(4),5,0,1,[]).
weapon(cluster_rocket_system,base,48,heavy('4d6'),5,0,1,[]).
weapon(destroyer_missile,base,60,heavy(1),nil,nil,nil,[inflicts_mortal_wounds(d3)
						      ,single_use
						      ,hit_on_a(6)]).
weapon(flamer,base,8,assault(d6),4,0,1,[auto_hits]).
weapon(pulse_blastcannon,close_range,10,heavy(2),14,-4,6,[]).
weapon(pulse_blastcannon,medium_range,20,heavy(4),12,-2,3,[]).
weapon(pulse_blastcannon,long_range,30,heavy(6),10,-3,d6,[]).
weapon(pulse_driver_cannon,base,72,heavy(d3),10,-3,d6,[if_target(models(10+),heavy(d6))]).
weapon(smart_missile_system,base,30,heavy(4),5,0,1,[non_visible_targets
						   ,strips_bonus(cover)]).
weapon(markerlight,base,36,heavy(1),nil,nil,nil,[markerlight]).
weapon(missile_pod,base,36,assault(2),7,-1,d3,[]).
weapon(pulse_carbine,base,18,assault(2),5,0,1,[]).
weapon(pulse_pistol,base,12,pistol(1),5,0,1,[]).
weapon(pulse_rifle,base,30,rapid_fire(1),5,0,1,[]).
weapon(photon_grenade,base,12,grenade(d6),nil,nil,nil,[photon_grenade]).
weapon(bolt_pistol,base,12,pistol(1),4,0,1,[]).
weapon(boltgun,base,24,rapid_fire(1),4,0,1,[]).
weapon(frag_grenade,base,6,grenade(d6),3,0,1,[]).
%eapon(Id,Profile,Range,Type,S,AP,D,Abilities)
weapon(krak_grenade,base,6,grenade(1),6,-1,d3,[]).



%!	faction_keyword(?Id, ?Keywords) is semidet.
%
%	Unit's faction keywords.
%
%	Each member of the list Keywords is an identifier for a clause
%	of keyword/n, storing the details of that keyword.
%
faction_keyword(lord_of_contagion, chaos).
faction_keyword(lord_of_contagion, nurgle).
faction_keyword(lord_of_contagion, heretic_astartes).
faction_keyword(lord_of_contagion, death_guard).
faction_keyword(kv128_stormsurge,tau_empire).
faction_keyword(kv128_stormsurge,<sept>).
faction_keyword(strike_team,tau_empire).
faction_keyword(strike_team,<sept>).
faction_keyword(tactical_drones,tau_empire).
faction_keyword(tactical_drones,<sept>).
faction_keyword(tactical_squad,imperium).
faction_keyword(tactical_squad,adeptus_astartes).
faction_keyword(tactical_squad,<chapter>).



%!	faction_keyword(?Id, ?Keywords) is semidet.
%
%	Unit's other (i.e. non-faction) keywords.
%
%	Each member of the list Keywords is an identifier for a clause
%	of keyword/n, storing the details of that keyword.
%
keyword(lord_of_contagion, infantry).
keyword(lord_of_contagion, terminator).
keyword(lord_of_contagion, character).
keyword(lord_of_contagion, lord_of_contagion).
keyword(kv128_stormsurge,vehicle).
keyword(kv128_stormsurge,titanic).
keyword(kv128_stormsurge,kv128_stormsurge).
keyword(strike_team,infantry).
keyword(strike_team,strike_team).
keyword(mv36_guardian_drone,drone).
keyword(mv36_guardian_drone,fly).
keyword(mv36_guardian_drone,guardian_drone).
keyword(tactical_drones,drone).
keyword(tactical_drones,fly).
keyword(tactical_drones,tactical_drones).
keyword(tactical_squad,infantry).
keyword(tactical_squad,tactical_squad).
