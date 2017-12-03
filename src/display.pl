:-module(display, [pretty_print/2
		  ]).

:-use_module(src(datasheets)).
:-use_module(project_root(configuration)).
:-use_module(src(wargear)).

/** <module> Predicates for display and visualisation.

*/

%!	overline_length(?Length) is det.
%
%	Length of section overlines
%
%	An "overline" is a separating line printed between sections to
%	help readability.
%
overline_length(90).


%!	pretty_print(+What, +Id) is det.
%
%	Pretty-print some information about a model or unit.
%
%	"What" is an identifier of the type of information to
%	pretty-print. This is one of:
%	* datasheet: Print a unit's datasheet. In this case, Id matches
%	the first argument of a unit_profiles/11 term in datasheets
%	module.
%
%
pretty_print(datasheet, Id):-
	unit(Id, _Name, Faction)
	,battlefield_role(Id, Role)
	,power(Id, Power)
	,unit_profiles(Id, Profiles)
	,damaged_profiles(Id, Dmg_profiles)
	/*,unit_composition(Id, Model_type, Number)
	,unit_composition_options(Id,Model,Number_in,Substitute,Number_out,Power_mod)*/
	,unit_wargear(Id, WGs)
	/*,wargear_options(Id,Item,Wg_Number_out,Wg_Substitute,Wg_Number_in)
	,abilities(Id, Abilities)
	,weapons(Id, Weapons)*/
	,unit_weapon_groups(WGs, Grouped)
	/*,faction_keyword(Id, Fc_Keywords)
	,keyword(Id, Keywords)*/
	,print_header(user_output,Id,Faction,Role,Power)
	,print_profiles(user_output,Profiles)
	,print_damaged_profiles(user_output,Dmg_profiles)
	,print_weapons(user_output,Grouped)
	.


%!	unit_profiles(+Id,-Profiles) is det.
%
%	Collect Profiles of all models in a unit.
%
%	Id should be the unit's identifier.
%
unit_profiles(Id, Profiles):-
	findall([Name_,M,WS,BS,S,T,W,A,Ld,Sv]
	       ,(unit_profiles(Id,Name,M,WS,BS,S,T,W,A,Ld,Sv)
		% Maybe leave that to printing preds?
		,printcase(Name, Name_)
		)
	       ,Profiles).


%!	damaged_profiles(+Id, -Profiles) is det.
%
%	Collect damage-induced modifiers to a unit's profile stats.
%
%	Id should be the identifier of a unit (not model).
%
damaged_profiles(Id, Profiles):-
	findall([Max_wounds,Min_wounds,Mods]
	       ,damaged_profiles(Id,Max_wounds,Min_wounds,Mods)
	       ,Profiles).


%!	unit_wargear(+Id, -Wargear) is det.
%
%	Collect default wargear options for a unit.
%
%	Id should be the identifier of a unit (not a model).
%
unit_wargear(Id, WGs):-
	findall(Wargear-Wg_Number
		,wargear(Id, Wargear, Wg_Number)
		,WGs).

%!	unit_weapon_groups(+Wargear,-Grouped) is det.
%
%	Group together a unit's Wargear with the same name.
%
%	This is used to group together different profiles of the same
%	weapon. Grouped is a list-of-lists where each list is either a
%	single element, meaning a weapon with a single profile, or
%	multiple elements, each one profile of the same weapon.
%
unit_weapon_groups(Wargear, Grouped):-
	findall(weapon(Wg_Id,Profile,Range,Type,Wp_S,AP,D,Wp_Abilities)
	       ,(member(Wg_Id-_Wg_Nums, Wargear)
		,weapon(Wg_Id,Profile,Range,Type,Wp_S,AP,D,Wp_Abilities)
		)
	       ,Weapons)
	,group_by_weapon_Id(Weapons,Grouped).


%!	print_header(+Stream,+Name,+Faction,+Battlefield_role,+Power)
%!	is det.
%
%	Print a header with a modoels Name etc basic information.
%
print_header(S,N,F,R,P):-
	configuration:format_string(print_header,_Ft)
        % Column length; totally eyballed.
	% Matches rest rows' columns.
	,Sp = 25
	% Convert text to printcase
	,findall(T_
		,(member(T, [N,F,P,R])
		 ,printcase(T, T_)
		 )
		,Ds)
	,flatten(Ds, [N_,F_,P_,R_])
	% Print unit name
	,format(S, '~*+~w~n',[Sp,N_])
	% Print separator line
	,print_overline(S,=)
	,format(S, '~w ~*+~w ~*+~w~n',['Role',Sp,'Power',Sp,'Faction'])
	,format(S, '~w ~*+~w ~*+~w~n',[R_,Sp,P_,Sp,F_])
	.


%!	print_profiles(+Stream, +Profiles) is det.
%
%	Print the profile of each model in a unit.
%
print_profiles(Str, Profiles):-
	Sp1 = 30 % Padding for Name column
	,Sp2 = 4 % Padding for each other column
	% Profile header text and padding
	,PH = ['Name',Sp1
	      ,'M',Sp2
	      ,'WS',Sp2
	      ,'BS',Sp2
	      ,'S',Sp2
	      ,'T',Sp2
	      ,'W',Sp2
	      ,'A',Sp2
	      ,'Ld',Sp2
	      ,'Sv']
	,print_overline(Str,-)
	,format(Str,'~w ~*+~w ~*+~w ~*+~w ~*+~w ~*+~w ~*+~w ~*+~w ~*+~w ~*+~w~n',PH)
	,forall(member([Name,M,WS,BS,S,T,W,A,Ld,Sv],Profiles)
	       ,(atomise(WS,WS_)
		,atomise(BS,BS_)
		,atomise(Sv,Sv_)
		% Profile values text and padding
		,P = [Name,Sp1
		     ,M,Sp2
		     ,WS_,Sp2
		     ,BS_,Sp2
		     ,S,Sp2
		     ,T,Sp2
		     ,W,Sp2
		     ,A,Sp2
		     ,Ld,Sp2
		     ,Sv_]
		,format(Str,'~w ~*+~w ~*+~w ~*+~w ~*+~w ~*+~w ~*+~w ~*+~w ~*+~w ~*+~w~n',P)
		)
	       ).


%!	print_damaged_profiles(+Stream, +Profiles) is det.
%
%	Tabulate a model's profile changes due to damage.
%
print_damaged_profiles(_, []):-
	!.
print_damaged_profiles(Str,Profiles):-
	Sp1 = 15
	,Sp2 = 4
	,PH = ['Remaining W'
	      ,Sp1,'BS'
	      ,Sp2,'S'
	      ,Sp2,'A'
	      ]
	,print_overline(Str,-)
	,format(Str,'~w ~*+~w ~*+~w ~*+~w ~n',PH)
	,forall(member([Max,Min,[BS,S,A]],Profiles)
	       ,(atomise(BS,BS_)
		,format(Str,'~w-~w ~*+~w ~*+~w ~*+~w ~n'
		       ,[Max,Min
			,Sp1,BS_
			,Sp2,S
			,Sp2,A
			])
		)
	       ).


%!	print_weapons(+Stream,+Weapons) is det.
%
%	Print a table of a unit's weapons' information.
%
print_weapons(Str,Weapons):-
	Sp1 = 30
	,atom_length('Range',Sp2)
	% assault(ndm) should be the longest weapon type string.
	,atom_length('assault(ndm)',Sp3)
	,PH = ['Weapon'
	      ,Sp1,'Range'
	      ,Sp2,'Type'
	      ,Sp3,'S'
	      ,Sp2,'AP'
	      ,Sp2,'D'
	      ,Sp2,'Abilities'
	      ]
	,print_overline(Str,-)
	,format(Str,'~w ~*+~w ~*+~w ~*+~w ~*+~w ~*+~w ~*+~w~n',PH)
	,forall(member(W,Weapons)
	       ,print_weapon_profiles(Str,W,Sp1,Sp2,Sp3)
	       ).

%!	print_weapon_profiles(+Stream,+Weapon,+Sep1,+Sep2,+Sep3) is det.
%
%	Print a line for each of a Weapon's profiles.
%
%	If Weapon has a single profile (i.e. its profile is listed as
%	"base") then a single line is printed, starting with the
%	Weapon's name. If Weapon has multiple profiles, the Weapon's
%	name is printed alone in one line, then each of its profiles are
%	listed underneath it as separate lines.
%
%	Abilities of Weapon are also printed, one on each line, using
%	the text specified in configuration:weapon_ability_text/2.
%
print_weapon_profiles(Stream,[weapon(Id,base,Range,Type,S,AP,D,Abilities)],Sp1,Sp2,Sp3):-
	!
	% Magic number used to print abilities in a column
	% starting a few characters away from where the last
	% weapon profile column ends. Totally eyballed but is
	% basically the length of all columns before Ability
	% plus a small offset.
	,Mn = 63
        ,printcase(Id, Id_)
	,P = [Id_,Sp1
	     ,Range,Sp2
	     ,Type,Sp3
	     ,S,Sp2
	     ,AP,Sp2
	     ,D,Sp2
	      % Print each ability
	     ,forall(member(Ability,Abilities)
			    ,(
			       (    configuration:weapon_ability_text(Ability,Frmt,Args)
			       ->   atomic_list_concat([~,Mn,+,Frmt,~,n],'',F)
			           ,format(Stream, F, Args)
			       ;    true
			      )
			     )
		    )
	      % If no abilities were printed, add an newline at the end
	     ,(	 Abilities = []
	      ->  nl(Stream)
	      ;	  true
	      )

	     ]
	% Not the format string doesn't have a newline at the end.
	,format(Stream,'~w~*+~w~*+ ~w~*+ ~w~*+ ~w~*+ ~w~*+ ~@~@',P).
print_weapon_profiles(Stream,Profiles,Sp1,Sp2,Sp3):-
	Profiles = [W|_]
	,W = weapon(Id,_,_,_,_,_,_,_)
	,printcase(Id, Id_)
	,Mn = 63
	,format(Stream,'~w~n',[Id_])
	,forall(member(weapon(_,Profile,Range,Type,S,AP,D,Abilities), Profiles)
	       ,(printcase(Profile,Profile_)
		,P = [Profile_,Sp1
		     ,Range,Sp2
		     ,Type,Sp3
		     ,S,Sp2
		     ,AP,Sp2
		     ,D,Sp2
		     ,forall(member(Ability,Abilities)
			     ,(configuration:weapon_ability_text(Ability,Frmt,Args)
			      ,atomic_list_concat([~,Mn,+,Frmt,~,n],'',F)
			      ,format(Stream, F, [Args])
			      )
			    )
		     ,(	 Abilities = []
		      ->  nl(Stream)
		      ;	    true
		      )
		     ]
		,format(Stream,'- ~w~*+~w~*+ ~w~*+ ~w~*+ ~w~*+ ~w~*+ ~@~@',P)
		)
	       ).



%!	print_overline(+Stream,+Character) is det.
%
%	Print a line of the given Character.
%
print_overline(S,A):-
	overline_length(Ovrln)
	,atom_codes(A,[C])
	,format(S, '~*t~*+~n', [C,Ovrln]).



%!	group_by_weapon_Id(+Weapons,-Grouped) is det.
%
%	Group a unit's weapons by their Id.
%
%	The thing is that grouped this way, weapons with the same Id but
%	_different profiles_ will be in the same sub-list of Grouped,
%	ready to be sub-tabluated accordingly.
%
group_by_weapon_Id(Ws,Gs):-
	findall(Ws_G
	       ,group_by(Id
			,W
			,(member(W,Ws)
			 ,W = weapon(Id,_Profile,_Range,_Type,_Wp_S,_AP,_D,_Wp_Abilities)
			 )
			,Ws_G)
	       ,Gs).



%!	printcase(+Atom, -Camel) is det.
%
%	Capitalise underscore-separated words in an Atom.
%
printcase(T, Tc):-
	atomic_list_concat(Ts, '_', T)
	,findall(Up
		,(member(W_, Ts)
		 ,atom_chars(W_,[C|Cs])
		 ,upcase_atom(C, C_)
		 ,atom_chars(Up,[C_|Cs])
		 )
		,T_)
	,atomic_list_concat(T_, ' ', Tc).



%!	atomise(+Tn,-Atomic) is det.
%
%	Conver a target number to an atom, for printing.
%
%	BS, WS, and Sv are given as target numbres, N+. They are easier
%	to print if passed to printing predicates as atoms, rather than
%	terms (because of operator precedence, I presume).
%
%	Additionally, some models' profiles have a "*" refering to a
%	damaged profile table, or a "-" meaning the model has no value
%	for that stat. These have to be dealt with also.
%
atomise(*, *):-
	!.
atomise(-, -):-
	!.
atomise(+(Tn),A):-
	atom_concat(Tn,+,A).
