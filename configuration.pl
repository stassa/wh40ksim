:-module(configuration, [model_characteristics/11
			]).



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

