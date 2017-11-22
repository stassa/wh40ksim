:-module(datasheets, [op(200,yf,+)
		     ,op(700,fy,<)
		     ,op(200,yf,>)
		     ,unit/3
		     ,battlefield_role/2
		     ,power/2
		     ,unit_profiles/10
		     ,damaged_profiles/4
		     ,unit_composition/2
		     ,unit_composition_options/2
		     ,wargear/3
		     ,wargear_options/2
		     ,abilities/2
		     ,weapons/2
		     ,weapon/8
		     ,faction_keywords/2
		     ,keywords/2
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
unit(kv128_stormsurge, 'KV128 Stormsurge', tau).




%!	battlefield_role(?ID, ?Role) is semidet.
%
%	A unit's battlefield role.
%
%	Used when making a Battle-forged army.
%
%	Symbols used on datasheets:
%	* Skull HQ
%	* Play Troops
%	* Skullcross Elites
%	* Thunder Fast Attack
%	* Explosion Heavy Support
%	* Skullarrow Dedicated Transport
%	* Wingsword Flyer
%	* Rook Fortification
%	* Gauntlet Lord of War
%
battlefield_role(lord_of_contagion, hq).
battlefield_role(kv128_stormsurge, lord_of_war).



%!	power(?ID, ?Power) is semidet.
%
%	A unit's Power level.
%
power(lord_of_contagion, 9).
power(kv128_stormsurge, 22).



%!	unit_profiles(?Id,?M,?WS,?BS,?S,?T,?W,?A,?Ld,?Sv) is semidet.
%
%	A unit's profiles - the characteristics of the unit's members.
%
%	When adding units to the database, use the following template to
%	quickly and accurately fill in unit details:
%	==
%	unit_profiles(Id,M,WS,BS,S,T,W,A,Ld,Sv)
%	==
%
unit_profiles(lord_of_contagion, 4, 2+, 2+, 4, 5, 6, 4, 9, 2+).
unit_profiles(kv128_stormsurge,6,5+,*,*,7,20,*,8,3+).



%!	damaged_profiles(?Id,?Max_wounds,?Min_wounds,?Mods) is det.
%
%	Changes to a unit's profile as damage accrues.
%
damaged_profiles(kv128_stormsurge,20,11,[4+,8,3]).
damaged_profiles(kv128_stormsurge,10,6,[5+,4,d3]).
damaged_profiles(kv128_stormsurge,5,1,[4+,8,3]).



%!	unit_composition(?Id, ?Composition) is semidet.
%
%	The number and types of models in a unit.
%
unit_composition(lord_of_contagion,single_model).
unit_composition(kv128_stormsurge,single_model).



%!	unit_composition_options(?Id, ?Option) is semidet.
%
%	Optional changes to a unit's composition.
%
unit_composition_options(lord_of_contagion, nil).
unit_composition_options(kv128_stormsurge, nil).



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



%!	wargear_options(?Id,?Options) is semidet.
%
%	Optional substitutions of a unit's wargear items.
%
%	Each Options is a compound, Item-N/Substitute-M, where Item is
%	the name of an item in the unit's wargear list, Substitute is
%	the name of an item that can replace it and N and M the numbers
%	of each item that can be substituted for each other.
%
wargear_options(kv128_stormsurge,flamer-2/burst_cannon-2).
wargear_options(kv128_stormsurge,flamer-2/airbursting_fragmentation_projector-2).
wargear_options(kv128_stormsurge,pulse_blastcannon-1/pulse_driver_cannon-1).
wargear_options(kv128_stormsurge,support_systems-3).



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
%
weapon(plaguereaper, base, melee, melee, +2, -3, 3, [plague_weapon]).
weapon(airbursting_fragmentation_projector,base,18,assault(d6),4,0,1,[non_visible_targets]).
weapon(burst_cannon,base,18,assault(4),5,0,1,[]).
weapon(cluster_rocket_system,base,48,heavy(4-d6),5,0,1,[]).
weapon(destroyer_missile,base,60,heavy(1),nil,nil,nil,[inflicts_mortal_wounds(d3)
						      ,single_use
						      ,hit_on_a(6)]).
weapon(flamer,base,8,assault(d6),4,0,1,[auto_hits]).
weapon(pulse_blastcannon,close_range,10,heavy(2),14,-4,6,[]).
weapon(pulse_blastcannon,medium_range,20,heavy(4),12,-2,3,[]).
weapon(pulse_blastcannon,long_range,30,heavy(6),10,-3,d-6,[]).
weapon(pulse_driver_cannon,base,72,heavy(d-3),10,-3,d-6,[if_target(models(10+),heavy(d6))]).
weapon(smart_missile_system,base,30,heavy(4),5,0,1,[non_visible_targets
						   ,strips_bonus(cover)]).



%!	faction_keywords(?Id, ?Keywords) is semidet.
%
%	A unit's faction keywords.
%
%	Each member of the list Keywords is an identifier for a clause
%	of keyword/n, storing the details of that keyword.
%
faction_keywords(lord_of_contagion, chaos).
faction_keywords(lord_of_contagion, nurgle).
faction_keywords(lord_of_contagion, heretic_astartes).
faction_keywords(lord_of_contagion, death_guard).
faction_keywords(kv128_stormsurge,tau_empire).
faction_keywords(kv128_stormsurge,<sept>).


%!	faction_keywords(?Id, ?Keywords) is semidet.
%
%	A unit's other (i.e. non-faction) keywords.
%
%	Each member of the list Keywords is an identifier for a clause
%	of keyword/n, storing the details of that keyword.
%
keywords(lord_of_contagion, infantry).
keywords(lord_of_contagion, terminator).
keywords(lord_of_contagion, character).
keywords(lord_of_contagion, lord_of_contagion).
keywords(kv128_stormsurge,vehicle).
keywords(kv128_stormsurge,titanic).
keywords(kv128_stormsurge,kv128_stormsurge).



