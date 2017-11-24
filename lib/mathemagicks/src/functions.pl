:-module(functions, [sigmoid/2
                    ,log2/2
                    ,logistic/5
                    ,logistic/8
                    ,logistic/9
                    ,product_of/3
                    ,sum_of/3
                    ]).

/* <module> Numeric functions for science, statistics, machine learning and so on.

Shhh. Don't say "numeric computing in Prolog". In any case, this module
is implemented entirely in pure Prolog and should not be considered
performant etc. You are welcome to use FORTRAN if you require
performance (or simply cannot abide with the eating of quiche).

*/


%!	sigmoid(+X,-Y) is det.
%
%	Logistic sigmoid.
%
%	Equivalent to logistic(X,1,1,0,Y), where:
%	==
%                  1
%	Y = -----------------
%              1 + exp(-X)
%	==
%
sigmoid(X,Y):-
    Y is 1 / (1 + exp(-X)).



%!	log2(N, L2_N) is det.
%
%	L2_N is the logarithm with base 2 of N.
%
log2(N, L2_N):-
	L_10 is log10(N)
	,L_2 is log10(2)
	,L2_N is L_10 / L_2.



%!	logistic(+X,+K,+L,+X0,-Y) is det.
%
%	Standard logistic function.
%
%       K is the steepness of the sigmoid curve; L is the sigmoid's
%       maximum; X0 is the sigmoid's midpoint.
%
logistic(X,K,L,X0,Y):-
    Y is L  / ( 1 + exp(-K * (X - X0)) ).



%!	logistic(+X,+A,+K,+B,+V,+Q,+C,-Y) is det.
%
%	Generalised logistic function.
%
%	@tbd Document additional parameters.
%
logistic(T,A,K,B,V,Q,C,Y):-
    must_be(between(0.0, inf), V)
    ,Y is A + ( (K - A) / ( C + (Q * exp(-B * T)) ) ^ 1/V ).



%!	logistic(+T,+A,+K,+B,+V,+Q,+M,+C,-Y) is det.
%
%	Generalised logistic, with more bells and whistles.
%
%	@tbd document parameters. _All_ of them.
%
logistic(T,A,K,B,V,Q,M,C,Y):-
    must_be(between(0.0, inf), V)
    ,Y is A + ( (K - A) / ( C + (Q * exp(-B *(T - M))) ) ^ 1/V ).



%!	product_of(+A,+B,-Product) is det.
%
%	Product of A and B.
%
%	Goes well with maplist, foldl et all.
%
product_of(A,B,C):-
	C is A * B.



%!	sum_of(+A,+B,-Sum) is det.
%
%       Sum of A and B
%
%	Convenience predicate to allow calling of library(apply)
%	predicates (and similar) with non-integer arguments (plus/3
%	only accepts integers).
%
sum_of(A, B, Sum):-
	Sum is A + B.
