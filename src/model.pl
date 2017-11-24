:-module(model, [model_value/3]).

:-use_module(project_root(configuration)).


%!	model_value(+Model, +Characteristic, -Value) is det.
%
%	Retrieve the Value associated with a Model's characteristic.
%
model_value(M, C, V):-
	% No idea what's going on here but current_functor/2
	% Gets some functors that are not defined anywhere,
	% particularly one that's arity-0 and one that has an
	% arity of 12 (for model_characteristics, always) that is
	% only defined in a commented-out line in configuration module.
	% The un-commented model_characteristics/11 is the only one
	% found by listing/1 anyway.
	current_functor(model_characteristics, A)
	,A \= 0
	,!
	,functor(T, model_characteristics, A)
	,configuration:T
	,arg(N,T,C)
	,arg(N,M,V).
