:-module(unit, [models_unit/3]).

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
