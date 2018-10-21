:-use_module(library(http/json_convert)).

:- json_object
	% Testing only.
	api_call(p:atom, a:number, b:number)
	,list_model_unit_sets(ms:list(atom), n:atom)
	.
