:-module(simulation, [number_of_attacks/4]).



%!	number_of_attacks(+M, +Pa, Wa, -N) is det.
%
%	Calculate the number of attacks for a set of models.
%
%	M is the number of models in the model-set, each of which share
%	a profile number of attacks and an equipped weapon type.
%
%	Pa is then the number of attacks listed in the model-set's model
%	profile and Wa is the number of attacks for their equipped
%	weapon type.
%
%	N is the produce M * Pa * Wa, the total number of attacks all
%	the models in the unit can attempt.
%
number_of_attacks(M, Pa, Wa, N):-
	N is M * Pa * Wa.
