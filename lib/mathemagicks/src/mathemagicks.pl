:-module(mathemagicks, [average/2
		       ,cumulative_distribution/3
		       ,cumulative_distribution/2
		       ,dot_product/3
		       ,float_between/4
		       ,frequencies/2
		       ,kullback_leibler_divergence/3
		       ,max_pair/2
		       ,min_pair/2
		       ,normalised_distribution/3
		       ,normalised_distribution/2
		       ,random_in_range/3
		       ,relative_frequencies/2
		       ,relative_frequency/3
		       ,run_length_encoding/2
		       ,sample_from_distribution/4
		       ,entropy/2
		       ,softmax/3
		       ,softmax/2
		       ,sum_terms/3
		       ]).

:-use_module(functions, [log2/2
			,sum_of/3
			 %^^ Actually used, but in a foldl/4 call
			 % that doesn't show up in the Swi IDE.
			,product_of/3
			]).

/** <module> Predicates for maths and counting things, a.k.a. statistics.

Predicates in this module are primarily used to operate on lists of
numbers or compounds (including key-value pairs as -/2 compounds). For
functions with a single numeric input and a single numeric output, see
module functions.

*/


%!	average(+Numbers,-Mean) is det.
%
%	Compute the arithmetic Mean of a list of Numbers.
%
average(Ns,A):-
	average(Ns,0,0,A).

average([],I,Sum,Avg):-
	Avg is Sum / I.
average([N|Ns],I,Acc,Bind):-
	succ(I,I_)
	,Acc_ is Acc + N
	,average(Ns,I_,Acc_,Bind).



%!	cumulative_distribution(+Values,+Index,?CDF) is det.
%
%	Calculate the cumulative distribution function of Values.
%
%	Values should be a list of terms representing probabilities or
%	events and their associated probabilities.
%
%	Index should be the numerical index of the argument representing
%	the probability value in each term. If Index is 0, then Values
%	should be a list of numbers only.
%
%	CDF is bound to a list where each probability in Values is
%	replaced by the sum of the probabilities preceding it in the
%	sorted list of Values.
%
%	This predicate makes no assumption, or attempt to verify, that
%	probability values sum to 1.0, that Index is really the index
%	of a numerical argument, that all terms in Values are of the
%	same format etc. As such, there is no guarantee that any error
%	raised when Values doesn't make sense, will, itself, make sense
%	(or even that an error will be raised at all).
%
%	Examples
%	--------
%	Values is a list of numbers:
%	==
%	?- cumulative_distribution([0.1,0.1,0.3,0.2,0.3], 0, Cs).
%       Cs = [0.1, 0.2, 0.4, 0.7, 1.0].
%	==
%
%	Values is a list of compound terms where the 3'd argument is a
%	probability value and the preceding arguments are the event
%	features:
%	==
%       ?- cumulative_distribution([e(x,y,0.1),e(y,x,0.1),e(z,y,0.3)
%           ,e(y,z,0.2),e(z,z,0.3)], 3, Cs).
%       Cs = [e(x, y, 0.1), e(y, x, 0.2), e(y, z, 0.4), e(z, y, 0.7) ,e(z, z, 1.0)].
%	==
%
%	Values is a list of key-value pairs where keys are events and
%	values their probabilities:
%	==
%	?- cumulative_distribution([a - 0.1, b - 0.1, c - 0.3, d - 0.2, e - 0.3], 2, Cs).
%       Cs = [a-0.1, b-0.2, d-0.4, c-0.7, e-1.0].
%	==
%
%	@tbd This should probably allow events represented as lists of
%	probabilities and features. For lists of more than two
%	elements, it most certainly currently doesn't. There's a bit of
%	a hitch in that Values must be sorted by I'th arguments and
%	sort/4 doesn't allow this for lists, so it will have to wait.
%
%	For the time being, be advised that lists of up to two elements
%	can be processed correctly, but with lists of more than two
%	elements you should expect an error, as in the following
%	example:
%	==
%	?- sort(1, @=<, [[1,e],[2,d],[3,c],[4,b],[5,a]], S).
%       S = [[1, e], [2, d], [3, c], [4, b], [5, a]].
%
%       ?- sort(2, @=<, [[1,e],[2,d],[3,c],[4,b],[5,a]], S).
%       S = [[5, a], [4, b], [3, c], [2, d], [1, e]].
%
%       ?- sort(2, @=<, [[1,e,4],[2,d,3],[3,c,2],[4,b,1],[5,a,5]], S).
%	S = [[5, a, 5], [4, b, 1], [3, c, 2], [2, d, 3], [1, e, 4]].
%
%       ?- sort(3, @=<, [[1,e,4],[2,d,3],[3,c,2],[4,b,1],[5,a,5]], S).
%       ERROR: argument `3' does not exist in [1,e,4]
%	==
%
cumulative_distribution(Vs,I,Cs):-
	(   I == 0
	 ->  msort(Vs,Vs_)
	    ,cumulative_sum(Vs_,0.0,[],Cs)
	 ;   I > 0
	->  sort(I,@=<,Vs,Vs_)
	   ,cumulative_sum(Vs_,I,0.0,[],Cs)
	;   throw('What do you mean I is negative?')
	 ).


%!	cumulative_sum(+Values,+Sum_acc,+Acc,-Sum) is det.
%
%	Business end of cumulative_distribution/2.
%
%	This is the sub-predicate used to calculate the cumulative Sum
%	of atomic Values.
%
cumulative_sum([],_,Cs,Cs_):-
	reverse(Cs,Cs_).
cumulative_sum([V|Vs],S_acc,Acc,Bind):-
	Vc is S_acc + V
	,cumulative_sum(Vs,Vc,[Vc|Acc],Bind).


%!	cumulative_sum(+Valuse,+Index,+Sum_acc,+Acc,-Sum) is det.
%
%	Business end of cumulative_distribution/3.
%
%	This is the sub-predicate used to calculate the cumulative Sum
%	of compound values, including associations.
%
cumulative_sum([],_,_,Cs,Cs_):-
	reverse(Cs,Cs_).
cumulative_sum([V|Vs],I,S_acc,Acc,Bind):-
	% TODO: use duplicate_term/2?
	V =.. [F|As]
	,nth1(I,As,K)
	,Vc is S_acc + K
	,V_ =.. [F|As]
	,nb_setarg(I,V_,Vc)
	,cumulative_sum(Vs,I,Vc,[V_|Acc],Bind).



%!	cumulative_distribution(+Terms,+Index) is det.
%
%	As cumulative_distribution/3 but updates Terms in-place.
%
%	Terms must be a list of compounds. Given the destructive update
%	performed in this predicate it doesn't make much sense to use it
%	with a list of atomic numbers. Use cumulative_distribution/3
%	instead if that's what you want to do.
%
%	Index is the position of the argument holding the values to be
%	replaced with the accumulated sum of their predecessors in each
%	compound in the sorted Terms list. This may not be 0.
%
%	Compounds' arguments are modified in-place, destructively and in
%	an icky, grotty, impure manner. You've been warned.
%
%	Examples
%	--------
%	==
%	?- Vs = [a-0.1, b-0.1, c-0.3, d-0.2, e-0.3], cumulative_distribution(Vs, 2).
%       Vs = [a-0.1, b-0.2, c-0.7, d-0.4, e-1.0].
%
%       ?- Vs = [e(x,y,0.1),e(y,x,0.1),e(z,y,0.3),e(y,z,0.2),e(z,z,0.3)]
%	    ,cumulative_distribution(Vs, 3).
%	Vs = [e(x, y, 0.1), e(y, x, 0.2) , e(z, y, 0.7), e(y, z, 0.4) ,e(z, z, 1.0)].
%
%       ?- Vs = [0.1,0.1,0.3,0.2,0.3], cumulative_distribution(Vs, 0).
%       ERROR: Type error: `compound' expected, found `0.1' (a float)
%	==
%
cumulative_distribution(Vs,I):-
	sort(I,@=<,Vs,Vs_)
	,once(cumulative_sum(Vs_,I,0.0)).


%!	cumulative_sum(+Values,+Index,+Accumulator) is det.
%
%	Business end of compounds_cumulative_distribution/2.
%
cumulative_sum([],_,_).
cumulative_sum([V|Vs],I,S_acc):-
	arg(I,V,K)
	,Vc is S_acc + K
	,nb_setarg(I,V,Vc)
	,cumulative_sum(Vs,I,Vc).



%!	dot_product(+Xs,+Ys,-Dot_product) is det.
%
%	Calculate the dot product of two lists of numbers.
%
%	Xs and Ys should be two lists of numbers. Dot_product is the
%	sum-of-products of those numbers, i.e. their dot product.
%
dot_product(Xs,Ys,Dp):-
	maplist(product_of, Xs, Ys, Ps)
	,foldl(sum_of, Ps, 0, Dp).



%!	float_between(+I,+J,+K,-F) is det.
%
%	Each F is a float between I and J, varying by K.
%
%	Similar to between/3 but will happily generate floats in the
%	closed interval [I,J] in strides of K.
%
%	Note that nothing constraints K to be a floating-point number
%	itself, so it's perfectly possible to generate floating point
%	numbers varying by an integer value.
%
float_between(I,J,K,F):-
	It = f(I)
	,repeat
	,arg(1,It,F)
	,(   F =< J
	 ->  F_ is F + K
	    ,nb_setarg(1,It,F_)
	 ;   !
	    ,fail
	 ).



%!	frequencies(+Sample,-Occurences) is det.
%
%	Count the Occurences of each event in a Sample.
%
%	Occurrences is a list of key-value pairs, where keys are the
%	events in Sample and values their frequencies.
%
frequencies(List,Frequencies):-
	% Sort List for faster aggregation of element counts
	sort(0,@=<,List,Sorted)
	% Count contiguous element "runs".
	% Since the List is sorted that's each element's total count
	,run_length_encoding(Sorted, Frequencies).



%!	relative_frequency(+Sample,+Event,-Relative_frequency) is nondet.
%
%	Find the Relative_frequency of an Event in a Sample.
%
%	Calculated by counting the occurences of Event in Sample and
%	dividing by the count of all events in Sample:
%	==
%	                      Event occurences
%	Relative frequency = ------------------
%	                        Sample size
%	==
%
relative_frequency(Ss, E, F):-
	findall(E, member(E,Ss), E_occurrences)
	,length(E_occurrences,N)
	,length(Ss,M)
	,F is N / M.



%!	run_length_encoding(+List, -Run_length_encoding) is det.
%
%	Converts a list to its run-length encoded form where each "run"
%	of contiguous repeats of the same element is replaced by that
%	element and the length of the run.
%
%	Run_length_encoding is a key-value list, where each element is a
%	term:
%
%	Element:term-Repetitions:number.
%
%	Example query:
%	==
%       ?- run_length_encoding([a,a,a,b,b,b,b,b,b,c,c,c,a,a,f], RLE).
%	RLE = [a-3, b-6, c-3, a-2, f-1].
%	==
%
run_length_encoding([], []-0):-
	!. % Green cut.
run_length_encoding([Head|List], Run_length_encoded_list):-
	run_length_encoding(List, [Head-1], Reversed_list)
	,reverse(Reversed_list, Run_length_encoded_list).


%!	run_length_encoding(+List,+Initialiser,-Accumulator) is det.
%
%	Business end of run_length_encoding/3. Calculates the run-length
%	encoded form of a list and binds the result to the Accumulator.
%	Initialiser is a list [H-1] where H is the first element of the
%	input list.
%
run_length_encoding([], Fs, Fs).
% Run of N consecutive identical elements
run_length_encoding([C|Cs],[C-F|Fs], Acc):-
        % Backtracking would produce successive counts
	% of runs of C at different indices in the list.
	!
	,F_ is F + 1
	,run_length_encoding(Cs, [C-F_| Fs], Acc).
% End of a run of N consecutive identical elements.
run_length_encoding([C|Cs], Fs, Acc):-
	run_length_encoding(Cs,[C-1|Fs], Acc).



%!	kullback_leibler_divergence(+P,+Q,-D) is det.
%
%	Compute the Kullback-Leibler Divergence of two models, P and Q.
%
%	This is calculated as:
%	==
%	D(P||Q) = Sum(Pi * log ( Pi / Qi) ) for all Pi in P, Qi in Q
%	==
%
%	P is taken to be the "true" distribution and Q the model. The
%	result is a measure of the amount of information lost when Q is
%	used to approximate P.
%
%	P and Q should be given as lists of compounds with any functor
%	and two arguments: the first should be an object in the model
%	and the second the object's probability.
%
kullback_leibler_divergence(P,Q,D):-
	% Sort without removing duplicates
	% To make it easier to walk the lists in tandem
	msort(P, P_)
	,msort(Q, Q_)
	%sort(1,@=<,P,P_)
	%,sort(1,@=<,Q,Q_)
	,aggregate(sum(KLD)
		  ,(tandem(P_,Q_,O_Pi-O_Qi)
		   ,O_Pi =.. [_,O,Pi]
		   ,O_Qi =.. [_,O,Qi]
		   ,(   number(Qi)
		    ->  M is Pi/Qi
		       ,log2(M, L)
		       ,KLD is Pi * L
		    ;   KLD is 0
		    )
		   )
		  ,D
		 )
	,!. % Greenish cut - too much backtracking over... ?


%!	tandem(+List1,+List2,-KVP) is det.
%
%	Iterate (well, recurse) over two lists in tandem, binding a
%	key-value pair for each of their elements.
%
%	This is similar to select/4 and similar, except the elements at
%	the same position in each list do not need to unify.
%
%	No check is made that the two lists are of equal length. If
%	List1 is shorter, tandem/3 will fail silently. If List2 is
%	shorter, tandem/3 will _succeed_ without warning. You need to
%	check that both lists are the same length before calling
%	tandem/3.
%
%	@tbd Copied from Experiments/mylibs because it's used in
%	kullback_leibler_divergence/3. Must find best way to reuse code
%	like that.
%
tandem([X|Xs],[Y|Ys],El) :-
        tandem(Xs,Ys,X-Y,El).

tandem(_, _, KVP, KVP).
tandem([X|Xs], [Y|Ys], _, Bind) :-
        tandem(Xs, Ys, X-Y, Bind).



%!	max_pair(+Pairs,-Max) is det.
%
%	Get the member of Pairs with the highest key.
%
max_pair(Ks,Max):-
	sort(1,@>=,Ks,[Max|_]).



%!	min_pair(+Pairs,-Min) is det.
%
%	Get the member of Pairs with the lowest key.
%
min_pair(Ks,Min):-
	keysort(Ks,[Min|_]).



%!	normalised_distribution(+Distribution,+I,-Normalised) is det.
%
%	Redistribute the probability mass of Distribution so that it
%	sums to 1.0
%
%	Distribution should be a list of compound terms representing
%	events in a probability distribution, where the I'th argument is
%	the probability and all remaining arguments the attributes of
%	the event.
%
%	Normalised is a list with the same events, with probabilities
%	normalised so that they sum to 1.0.
%
normalised_distribution(Ds, I, Ds_):-
	sum_terms(Ds,I,S)
	,findall(E_
		,(member(E,Ds)
		 ,E =.. [F|As]
		 ,nth1(I,As,P)
		 ,Pn is P / S
		 ,select(P,As,Pn,As_)
		 ,E_ =.. [F|As_]
		 )
		,Ds_).



%!	normalised_distribution(+Distribution,+I) is det.
%
%	Normalise Distribution so that its probabilities sum to 1.
%
%	This is the destructive-assignment version of
%	normalised_distribution/3, meaning that terms in Distribution
%	are updated in place. Use normalised_distribution/3 if you wish
%	to retain the original probabilities in your data.
%
normalised_distribution(Ds, I):-
	sum_terms(Ds,I,S)
	,forall(member(E,Ds)
	       ,(arg(I,E,P)
		,Pn is P / S
		,nb_setarg(I,E,Pn)
		)
	       ).



%!	random_in_range(+Min,+Max,-Random) is det.
%
%	Get a Random float in the open interval (Min,Max).
%
%	This uses random/1 to generate a random R in the open interval
%	(0.0,1.0) then multiplies R by the distance from Min to Max and
%	shifts the value of R by Min:
%	==
%	random(R)
%	,Random is (Max - Min) * R + Min
%	==
%
random_in_range(Min,Max,R_):-
	random(R)
	,R_ is (Max-Min) * R + Min.



%!	random_in_inclusive_range(+Min,+Max,-Random) is det.
%
%	Get a Random float in the closed interval [Min,Max].
%
%	This uses random/1 to generate a random R in the open interval
%	(0.0,1.0) then multiplies R by the distance from Min to Max and
%	shifts the value of R by Min:
%	==
%	random(R)
%	,Random is (Max - Min) * R + Min
%	==
%
%	@tbd Doesn't quite work and is not immediately necessary.
%
random_in_inclusive_range(Min,Max,R_):-
	random(R)
	,D is (Max - Min) +  0.0000000000000002
	,Min_ is Min - 0.0000000000000001
	,R_ is D * R + Min_.




%!	relative_frequencies(+Characters,-Frequencies) is det.
%
%	Calculates the relative frequencies of elements in the list of
%	Characters.
%
%	Frequencies is a key-value list with elements of the form:
%	C-F, where C a character in the list and F its relative
%	frequency in the list.
%
%	Example query:
%	==
%	?- relative_frequencies([a,a,a,b,b,b,b,b,b,c,c,c,a,a,f], Fs).
%	Fs = [a-0.3333333333333333, b-0.4, c-0.2,f-0.06666666666666667].
%	==
%
relative_frequencies(List, Frequencies):-
	/* This re-uses element count aggregation from frequencies/2
	but it adds the frequency-over-totals division in the findall/3
	loop to avoid walking again over the list of element counts. */
	sort(0,@=<,List,Sorted)
	,run_length_encoding(Sorted, RLEs)
	,length(List, Elements_in_list)
	% Aggregate grouped lengths
	,findall(E-Frequency_of_E
		,(member(E-Occurences_of_E, RLEs)
		 ,Frequency_of_E is Occurences_of_E / Elements_in_list
		 )
		,Frequencies).



%!	softmax(+Terms,+Index,-Softmaxed) is det.
%
%	Implements the softmax, or normalised exponential, function.
%
%	Index is the 1-based index of the argument holding the values to
%	be softmaxed for each term in Terms. If Index is 0, Terms should
%	be a list of atomic numbers.
%
%	Softmaxed is bound to the same list of input Terms but with
%	their I'th values in Terms squashed so that they lie between 0.0
%	and 1.0 but occupying the same amount of space relatively to
%	each other as they did before.
%
softmax(Vs, 0, Vs_):-
	!
	,findall(V_
	       ,(member(V,Vs)
		,V_ is exp(V)
		)
	       ,Vs_exp)
	,sum_list(Vs_exp, S)
	,findall(Vi
		,(member(Ei,Vs_exp)
		 ,Vi is Ei / S
		 )
		,Vs_).

softmax(Vs,I,Vs_):-
	once(exponents_and_sum(Vs, I, 0.0, [], Vs_, S))
	% Note destructive assignment -in Vs_ only though.
	% Input (Vs) is not modified anywhere.
	,forall(member(Ex,Vs_)
	       ,(arg(I,Ex,K_ex)
		,K_ is K_ex / S
		,nb_setarg(I,Ex,K_)
		)
	       ).


%!	exponents_and_sum(+Terms,+I,+Sum_acc,+Exp_acc,-Exponents,-Sum)
%!	is det.
%
%	Business end of softmax/3.
%
%	Also, the name of a pub in Greenwich where mathematicians like
%	to drink.
%
%	Exponents is bound to the list of input Terms, but with their
%	I'th argument replaced by the exponent of the original. Sum is
%	the sum of these exponent values.
%
%	Used to avoid getting the list of exponents and then summing
%	them separately.
%
exponents_and_sum([], _, S, Ex, Ex_, S):-
	!
	,reverse(Ex,Ex_).
exponents_and_sum([V|Vs],I,S_acc,Ex_acc,Ex_bind,S_bind):-
	V =.. [F|As]
	,nth1(1,As,K)
	,K_ex is exp(K)
	,V_ex =.. [F|As]
	,nb_setarg(I,V_ex,K_ex)
	,S_ is S_acc + K_ex
	,exponents_and_sum(Vs,I,S_,[V_ex|Ex_acc],Ex_bind,S_bind).



%!	softmax(+Terms, +Index) is det.
%
%	Destructive assignment version of softmax/3.
%
%	Operates only on compounds; use softmax/3 with I = 0 to
%	process a list of atomic numbers.
%
%	The softmax'd value of the I'th argument of each compound in
%	Terms is bound to that position. Terms are modified _in place_.
%	_Destructively_. This is _destructive assignment_. You are now
%	leaving the safe zone of pure Prolog. Chaos may ensue. The sky
%	may fall. Edgar Dijkstra will most definitely rise up from his
%	grave and say "I told you so!" (even though he never did).
%	Laplace, Gauss and Fermat will definitely look down on you.
%	Hilbert, Panini and Aristotle will be most displeased. You will
%	be shunned by your comrades at arms in the few remaining
%	bastions of logic programming and your spouse will have a
%	disappointed smirk on his or her face for ever more. Your cat
%	will hiss at you. Your dog will shed its coat out of season. A
%	two-headed goat with three horns will be seen in the countryside
%	east of Glasgow. Tory voters will vote Labour and Labour voters
%	vote Tory. THE WORLD WILL END!!!
%
%	You have now been warned.
%
softmax(Vs, I):-
	S = s(0.0)
	,forall(member(V,Vs)
	       ,(arg(1,S,Sx)
		,arg(I,V,K)
		,K_ is exp(K)
		,Sx_ is Sx + K_
		,nb_setarg(1,S,Sx_)
		% NOTE: we're also setting the compound value
		% to the exponent of its former one. Destructively.
		,nb_setarg(I,V,K_)
		)
	       )
	,arg(1,S,Sk)
	,forall(member(V,Vs)
	       ,(arg(I,V,J)
		,J_ is J / Sk
		,nb_setarg(I,V,J_)
		)
	       ).



%!	sample_from_distribution(+P,+I,+Distribution,-Event) is det.
%
%	Select a random Event from a Distribution.
%
%	Distribution should be a list of compound terms representing
%	observations with a probability given as the I'th argument, and
%	the observation's features represented by remaining arguments.
%
%	For instance, let Ds be a list of compounds: d(Pi,Hi,Wi), where
%	Pi the probability, Hi the height and Wi the weight of an
%	individual i, in a set of biometric data. Then, a query to draw
%	a sample from this data would set I to 1:
%	==
%	?- ..., sample_from_compound_distribution(R,1,Ds,S).
%       ==
%
%	Event is bound to the first term in Distribution whose
%	_cumulative_ probability is equal to, or higher than, P, so
%	that, if Pi is the cumulative probability of event Ei and Pk the
%	cumulative probability of event Ek, immediately following Ei in
%	the list Distribution, Ek is selected if:
%	==
%       Pi <= P < Pk
%	==
%
%	An error will be raised if the sum of probabilities in
%	Distribution is more than 1.0.
%
%	@bug The in-line calculation of cumulative probabilities will
%	break down when two or more events have the same probability
%	going in (one of them will have the correct one and subsequent
%	events will get increasingly higher values). This becomes more
%	likely when probability values in Distribution are represented
%	by floating point numbers rounded to only a couple of decimals.
%
sample_from_distribution(P,I,Ds,S):-
	sort(I,@=<,Ds,Ds_)
	,search_distribution(P,I,0.0,Ds_,S).


%!	search_distribution(+P,+I,+Cumulative,+Distribution,-Event) is det.
%
%	Business end of sample_from_distribution/4.
%
%	Search Distribution for an Event with the given probability, P.
%	Event is a compound term where the I'th argument represents the
%	probability of an observation with features listed in the
%	remaining arguments of the compound.
%
%	An Event is selected if its Cumulative probability (the sum of
%	its own probability value and the cumulative probabilities of
%	all preceding events in the sorted Distribution list) is equal
%	to or higher than the given probability, P.
%
search_distribution(P,I,CP,[Ei|_Es],Ei):-
	arg(I,Ei,Pi)
	,CPi is CP + Pi
	,CPi >= P
	,!.
search_distribution(P,I,CP,[Ei|Es],Acc):-
	arg(I,Ei,Pi)
	,CPi is CP + Pi
	,!
	,search_distribution(P,I,CPi,Es,Acc).
search_distribution(_P,_I,CP,_Es,_Acc):-
	format(atom(A)
	      ,'Ran out of probability mass at ~w'
	      ,[CP])
	,throw(A).



%!	entropy(+String, -Entropy) is det.
%
%	Calculate the shannon Entropy of String.
%
%	Example query:
%	==
%	?- shannon_entropy(1223334444, H).
%	H = 1.8464393446710154.
%	==
%
entropy(String, Entropy):-
	atom_chars(String, Cs)
	,relative_frequencies(Cs, Frequencies)
	,findall(CI
		,(member(_C-F, Frequencies)
		 ,log2(F, L)
		 ,CI is F * L
		 )
		,CIs)
	,foldl(sum_of, CIs, 0, E)
	,Entropy is -E.



%!	sum_terms(+Terms,+I,-Sum) is det.
%
%	Sum the I'th argument of each compound in Terms.
%
sum_terms(Ts,I,S):-
	Acc = s(0)
	,forall(member(T,Ts)
	       ,(arg(I,T,V)
		,arg(1,Acc,S_acc)
		,S_acc_ is S_acc + V
		,nb_setarg(1,Acc,S_acc_)
		)
	       )
	,arg(1,Acc,S).
