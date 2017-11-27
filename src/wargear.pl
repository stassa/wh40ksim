:-module(wargear, [weapon_value/3]).

:-use_module(src(datasheets)).

/** <module> Predicates to implement weapon logic.

Unfortunately, such functionality is necessary for the Greater Good.

*/


%!	weapon_value(+Weapon_name,+Characteristic,-Value) is det.
%
%	Retrieve the Value associated with a Weapon's Characteristic.
%
weapon_value(Wg, C, V):-
	W = weapon(Wg,_Profile,_Range,_Type,_S,_AP,_D,_Abilities)
	,W
	,current_functor(weapon_characteristics, A)
	,A \= 0
	,functor(T, weapon_characteristics, A)
	,configuration:T
	,arg(N,T,C)
	,!
	,arg(N,W,V).
% Hack until non-weapon wargear like shield generators are handled
% properly.
weapon_value(_, 'Range', 0):-
	!.
weapon_value(_, _, none(0)).
