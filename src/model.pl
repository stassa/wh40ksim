:-module(model, [characteristic_index/2
		,model_value/3]).

:-use_module(project_root(configuration)). % Yes it is.


%!	characteristic_index(+Characteristic, -Index) is det.
%
%	The Index of Characterstic  in model/N terms.
%
%	Note that model/N terms are never asserted to the database, e.g.
%	as part of the datasheet information in datasheets model.
%	Instead, they are internal representations of models,
%	constructed by and passed around predicates in this project.
%	Although these model/N terms will always have the same structure
%	regardless of the predicate manipulating them, their definition
%	may change according to the needs of the project as it develops,
%	so their characteristics and values should always be accessed
%	using this and other interface predicates, such as
%	model_value/3.
%
characteristic_index(C, I):-
	current_functor(model_characteristics, A)
	,A \= 0
	,!
	,functor(T, model_characteristics, A)
	,configuration:T
	,arg(I,T,C).



%!	model_value(+Model, +Characteristic, -Value) is det.
%
%	Retrieve or change the Value of a Model's Characteristic.
%
%	If Value is a variable, then the current value associated with
%	the given Characteristic is bound to Value. If Value is not a
%	variable, that Characteristic's value is updated destructively
%	and in a non-backtrackable manner (i.e. backtracking will not
%	undo the bindings and change the value anew).
%
model_value(M, C, V):-
	% No idea what's going on here but current_functor/2
	% Gets some functors that are not defined anywhere,
	% particularly one that's arity-0 and one that has an
	% arity of 12 (for model_characteristics, always) that is
	% only defined in a commented-out line in configuration module.
	% The un-commented model_characteristics/11 is the only one
	% found by listing/1 anyway.
	\+ ground(V)
	,current_functor(model_characteristics, A)
	,A \= 0
	,functor(T, model_characteristics, A)
	,configuration:T
	,arg(N,T,C)
	,! % Grrrr.
	,arg(N,M,V).
model_value(M, C, V):-
	nonvar(V)
	,current_functor(model_characteristics, A)
	,A \= 0
	,functor(T, model_characteristics, A)
	,configuration:T
	,arg(N,T,C)
	,! % Grrrr.
	,nb_setarg(N,M,V).
