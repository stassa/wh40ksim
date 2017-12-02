:-module(configuration, [format_string/2
			,model_characteristics/11
			,weapon_characteristics/8
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


%!	wound_allocation_strategy(?Strategy) is det.
%
%	A Strategy for allocating wounding hits to a unit's models.
%
%	Relevant to unit:wound_allocation_order/3, used to select
%	clauses according to a preferred Strategy.
%
wound_allocation_strategy(fewer_wounds_first).
