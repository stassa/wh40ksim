:-module(configuration, [format_string/2
			,model_characteristics/11
			,weapon_characteristics/8
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
