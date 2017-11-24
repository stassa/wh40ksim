:-module(die_size, [die_size/3
		   ,target_number/2
		   ,roll/3
		   ,roll/2
		   ,roll_vs_tn/5
		   ,roll_vs_tn/4
		   ,roll_vs_tn_mod/6
		   ,roll_vs_tn_mod/5
		   ]).


%!	die_size(?Notation, ?K, ?N) is det.
%
%	Parse, or generate a die size in KdM Notation.
%
die_size(NDM, N, M):-
	atomic_list_concat([N_,M_],d,NDM)
	,(   N_ = ''
	 ->  N = 1
	 ;   atom_number(N_, N)
	 )
	,atom_number(M_, M).



%!	target_number(?Target_number, -N) is det.
%
%	Parse a Target_number in N+ notation.
%
%	This currently only accepts N+ target numbers (not N- or just N)
%	because I've only seen that kind of target number so far. Will
%	probably need to extend eventually.
%
%	Yeah, definitely- and I should add an argument to carry out the
%	relation we want to check for, >, <, =<, >= or ==, which can
%	then be used in roll_vs_tn/[4,5] and roll_vs_tn_mod/[5,6].
%	Currently, these two only do roll-over target numbers (N+).
%
target_number(Tn, N):-
	Tn =.. [+, N].



%!	roll(+K, N, -R) is det.
%
%	Roll K dice of size N, summing to R.
%
roll(K, N, M):-
	aggregate(sum(Mi)
		 ,I
		 ,(between(1,K,I)
		  ,random_between(1,N,Mi)
		  )
		 ,M).



%!	roll(+KdN, -R) is det.
%
%	Roll K dice of size N, summing to R.
%
%	Alterantive to roll/3 accepting KdN notation.
%
roll(KdN, M):-
	die_size(KdN, K, N)
	,aggregate(sum(Mi)
		 ,I
		 ,(between(1,K,I)
		  ,random_between(1,N,Mi)
		  )
		 ,M).



%!	roll_vs_tn(+K, +J, +N, +Tn, -R) is det.
%
%	Roll K times JdN and count R hits against Tn.
%
roll_vs_tn(K, J, N, Tn, R):-
	aggregate(sum(H)
		 ,(I, M)
		 ,(between(1, K, I)
		  ,roll(J,N,M)
		  ,(   M >= Tn
		      ,M > 1
		   ->  H = 1
		   ;   H = 0
		   )
		  )
		 ,R).



%!	roll_vs_tn(+K, +JdN, +Tn, -R) is det.
%
%	Roll K times JdN and count R hits against Tn.
%
%	Alternative to roll_vs_tn/5 accepting dice in JdN notation and
%	target number in N+ notation.
%
%	@tbd This and subsequent predicates testing a roll against a
%	target number count unmodified rolls of 1 as fa failure. This
%	seems to be a pretty consistent rule throughout combat, at
%	least- but watch out for exceptions, which might require a lot
%	of fiddly exception code.
%
roll_vs_tn(K, JdN, T, R):-
	target_number(T, Tn)
	,aggregate(sum(H)
		 ,(I, M)
		 ,(between(1, K, I)
		  ,roll(JdN,M)
		  ,(   M >= Tn
		      ,M > 1
		   ->  H = 1
		   ;   H = 0
		   )
		  )
		 ,R).



%!	roll_vs_tn_mod(+K, +J, +N, +T, +M, -R) is det.
%
%	Roll K times JdN against a target number of T, modified by M.
%
roll_vs_tn_mod(K, J, N, Tn, Mod, R):-
	aggregate(sum(H)
		 ,(I, M, M_)
		 ,(between(1, K, I)
		  ,roll(J,N,M)
		  ,M_ is M + Mod
		  ,(   M_ >= Tn
		   % Note test for _unmodified_ 1.
		      ,M > 1
		   ->  H = 1
		   ;   H = 0
		   )
		  )
		 ,R).



%!	roll_vs_tn_mod(+K, +JdN, +T, +M, -R) is det.
%
%	Roll K times JdN against a target number of T, modified by M.
%
%	Alternative to roll_vs_tn_mod/6 accepting die sizes in JdN
%	notation and target numbers in T+ notation.
%
roll_vs_tn_mod(K, JdN, T, Mod, R):-
	target_number(T, Tn)
	,aggregate(sum(H)
		 ,(I, M, M_)
		 ,(between(1, K, I)
		  ,roll(JdN,M)
		  ,M_ is M + Mod
		  ,(   M_ >= Tn
		      ,M > 1
		   ->  H = 1
		   ;   H = 0
		   )
		  )
		 ,R).







