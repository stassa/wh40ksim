:-module(web_api, [start_server_at/1]).

:- use_module(library(http/thread_httpd), [http_server/2]).
:- use_module(library(http/http_dispatch)). % http_dispatch meta-call in http_server/2.
:- use_module(library(http/http_json), [reply_json/1
				       ,http_read_json_dict/2
				       ,http_read_json/3
				       ]).
:- use_module(library(http/http_client), [http_read_data/3]).
:-use_module(library(http/json_convert)).

% Loaded from load_project for dev.
% Uncomment for prod.
%:-[web(json_objects)].


start_server_at(Port):-
	http_server(http_dispatch, [port(Port)]).

:- http_handler(root(api), api, []).

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
	,http_read_json_dict(Req, Data)
	,writeln(Data)
	,atom_string(Functor, Data.functor)
	,Call =.. [Functor|Data.args]
	,scripts:call(Call, Res)
	,format('~w~n',[Res])
	,nl.
api_(Req):-
	member(method(post), Req)
	,!
	,format('Content-type: text/plain~n~n', [])
	,http_read_json(Req, Data, [json_object(term)])
	,json_to_prolog(Data,Call)
	,writeln(Data)
	,writeln(Call)
	%,call(Call, Res)
	%,format('~w~n',[Res])
	,nl.

% Nah, can't do this.
%api_call(As, Res):-
%	append(As,Res,As_)
%	,C =.. [call|As_]
%	,C.

api_call(P,A,B,C):-
	call(P,A,B,C).

api_add(A,B,C):-
	C is A + B.

api_sub(A,B,C):-
	C is A - B.
