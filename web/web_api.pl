:-module(web_api, [start_server_at/1]).

:- use_module(library(http/thread_httpd), [http_server/2]).
:- use_module(library(http/http_dispatch)). % http_dispatch meta-call in http_server/2.
:- use_module(library(http/http_json), [reply_json/1
				       ,http_read_json_dict/3
				       ,http_read_json/3
				       ]).
:- use_module(library(http/http_client), [http_read_data/3]).
:-use_module(library(http/json_convert)).
:-use_module(scripts/scripts).

%!	start_server_at(+Port) is det.
%
%	Start the web api server at the listed Port.
%
start_server_at(Port):-
	http_server(http_dispatch, [port(Port)]).


:- http_handler(root(api), api, []).

%!	api(+Request) is det.
%
%	Handles an api Request.
%
api(Req):-
	member(method(get), Req)
	,!
	,http_read_data(Req, Data, [])
	,reply_json(Data)
	,nl.
api(Req):-
	member(method(post), Req)
	,!
	,format('Content-type: text/plain~n~n', [])
	,http_read_json_dict(Req, Data, [value_string_as(atom)])
	%,writeln(Data)
	,api_call(Data.out_vars, Data, Res)
	,(   nonvar(Res)
	->   format('~w~n',[Res])
	 ;   true
	 )
	,nl.


%!	api_call(+Out,+Data,-Result) is det.
%
%	Satisfy a post request.
%
%	Out is the number of arguments used for "output". This number is
%	used to select clauses and pass the predicate functor and
%	arguments to the correct-arity call/n version.
%
%	Data is the data received from a POST request, given as an
%	anonymous dict, with elements:
%	==
%	_N{functor:F,args:List,out_vars:M}
%	==
%
%	Where F is the functor name of the predicate to be called, List
%	is a list of arguments to be passed to that predicate and M the
%	same as Out.
%
%	api_call/3 will take the functor name and arguments out of the
%	dict, Data, select the correct call/n clause to perform the
%	call and bind the results to Result.
%
%	Note that if Out is 0, Result will remain unbound and if Out is
%	more than 1 Result will be a list. Otherwise, it will be
%	whatever is the result of calling Functor/Args.
%
api_call(0, Data, _):-
	!
	,atom_string(Functor, Data.functor)
	,Call =.. [Functor|Data.args]
	,call(Call).
api_call(1, Data, Res):-
	!
	,atom_string(Functor, Data.functor)
	,Call =.. [Functor|Data.args]
	,call(Call, Res).
api_call(2, Data, Res):-
	atom_string(Functor, Data.functor)
	,Call =.. [Functor|Data.args]
	,call(Call, Res1,Res2)
	,append(Res1,Res2,Res).
