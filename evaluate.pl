%%%%%% Auswertung von Sch�lerl�sungen




:- ensure_loaded('parameters.pl').
:- ensure_loaded('auxprd.pl').



%%% eval_exercise(+Tag, +RR, +Exm, +UM, -OR, -Points, -Reached)
% Wertet eine durch Tag bezeichnete Aufgabe aus. Gibt in OR die gefundenen Fehler und
% Effizienzdaten zur�ck, sowie in Points die erreichbare und in Reached die erreichte
% Punktzahl (wobei Abz�ge f�r gegebene Hinweise noch nicht ber�cksichtigt sind).

eval_exercise(Tag, [UserFN, RPI| RRT], Exm, [Username| _], OR, Points, Reached) :-

	access_eval_file([], Username, OR01, Stop01),
	find_lisp_file(OR01, LispFN, OR02, Stop02),
	(member(true, [Stop01, Stop02]), OR = OR02)>+

	find_tag(Tag, Exm, [_, Directives, ExmLines], ExmFN), !,
	find_tag(Tag, [[UserFN, RPI| RRT]], [_, _, Lines], _), !,

	examine_directives(Directives, [], Function, Equivalence, ParmLists, Repeat, Points, OR1, Stop1),
	(Stop1 = true, OR = OR1)>+

	% M�gliche verwendete Operatoren finden
	get_objects(ExmLines, 0, possible_operator, ExmPOps, _, true),
	get_objects(Lines, 0, possible_operator, POps, _, true),

	create_eval_file(Username, UserFN, ExmFN, Function,
		Equivalence, ParmLists, Repeat, POps, ExmPOps),
	construct_lisp_expr(Username, LispFN, Expr),
	lisp_eval(Expr, Result),
	(Result \= 0, OR = [lisp_eval])>+

	analyse_errorfile(Username, [], OR2, Reached, Stop2),
	(Stop2 = true, OR = OR2)>+

	tracefile_name(Username, TraceFN),
	analyse_tracefile(TraceFN, OR2, OR, Points, Reached, _).


%%% access_eval_file(+OR1, +Username, -OR2, -Stop)
% Pr�ft, ob das Auswertungsprogramm geschrieben und gelesen werden kann.

access_eval_file(OR0, Username, OR0, false) :-
	evalfile_name(Username, EvalFN),
	access_file(EvalFN, write),
	tell(EvalFN), told,
	access_file(EvalFN, read), !.
access_eval_file(OR0, Username, [file_error(access, EvalFN)| OR0], true) :-
	evalfile_name(Username, EvalFN).


%%% find_lisp_file(+OR1, -LispFN, -OR2, -Stop)
% Pr�ft, ob das Lisp-Hilfsprogramm existiert und lesbar ist

find_lisp_file(OR0, LispFN, OR, Stop) :-
	base_path(LispPath, "evaluate.lsp"),
	name(LispFN, LispPath),
	(access_file(LispFN, read),
		OR = OR0,
		Stop = false)>+
	Stop = true,
	(access_file(LispFN, exist), !,
	  OR = [file_error(read, LispFN)| OR0];
	  OR = [file_error(exist, LispFN)| OR0]).


%%% possible_operator(+Line_tail, -POp)
% Hilfspr�dikat von eval_exercise, zum �bergeben an get_objects.

possible_operator([C, [sym, X, _]| _], X) :-
	opening_parenthesis(C).


%%% examine_directives(+Directives, +OR1,
%%%	-Function, -Equivalence, -ParmLists, -Repeat, -Points, -OR2, -Stop)
% Hilfspr�dikat von eval_exercise. Pr�ft die Aufgabendirektiven, die f�r die Evaluierung
% relevant sind, auf Vollst�ndigkeit und gibt ggf. deren Werte zur�ck.

examine_directives(Directives, OR0,
		Function, Equivalence, ParmLists, Repeat, Points, OR, Stop) :-

	% Funktionssymbol
	function_marker(FM),
	(member(d([FM, Function| _]), Directives), !,
	 OR1 = OR0;
	 OR1 = [exm_error(missing_dir, function)| OR0]),

	% Symbol der �quivalenzfunktion
	equivalence_marker(EM),
	(member(d([EM, Equivalence| _]), Directives), !,
	 OR2 = OR1;
	 OR2 = [exm_error(missing_dir, equivalence)| OR1]),

	% Testparameterlisten
	test_parameters_marker(TPM),
	findall(Parms1,
	 	(member(d([TPM| Parms]), Directives), concat_atom(Parms, ' ', Parms1)),
	 	ParmLists
	),
	(ParmLists \= [], !,
	 OR3 = OR2;
	 OR3 = [exm_error(missing_dir, test_parameters)| OR2]),

	% Erreichbare Punktzahl
	points_marker(PM),
	(member(d([PM, Points| _]), Directives), !,
	 OR4 = OR3;
	 OR4 = [exm_error(missing_dir, points)| OR3]),

	% Wiederholungsparameter
	repeat_marker(RM),
	(member(d([RM, Repeat| _]), Directives);
	 Repeat = 0),

	OR = OR4,
	(OR = [], Stop = false)>+
	Stop = true.



%%% create_eval_file(+EvalFN, +Username, +UserFN, +ExmFN, +Function, +Equivalence,
%%%	+ParmLists, +Repeat, +POps, +ExmPOps) :-
% Hilfspr�dikat von eval_exercise. Schreibt eine Lisp-Datei, bei deren Laden
% die Benutzerfunktion ausgewertet wird.

create_eval_file(Username, UserFN, ExmFN, Function, Equivalence,
		ParmLists, Repeat, POps, ExmPOps) :-
	concat_atom(ParmLists, ' ', ParmLists1),
 	concat_atom(ExmPOps, ' ', ExmPOps1),
 	concat_atom(POps, ' ', POps1),
	evalfile_name(Username, EvalFN),
	tracefile_name(Username, TraceFN),
	opsfile_name(Username, OpsFN),
	concat_atom(
	 ['(evaluate "', UserFN, '" "', ExmFN, '" "',
	   TraceFN, '" "', OpsFN, '" ', Function, ' ', Equivalence, ' (',
	   ParmLists1, ') (', ExmPOps1, ') (', POps1, ') ', Repeat, ')'],
	 Prog),
	tell(EvalFN),
	write(Prog),
	told.


%%% construct_lisp_expr(+Username, +LispFN, -Expr) :-
% Hilfspr�dikat von eval_exercise. Konstruiert den Ausdruck, der an den
% Lisp-Interpreter �bergeben wird, um die Benutzerfunktion auszuwerten.

construct_lisp_expr(Username, LispFN, Expr) :-
	evalfile_name(Username, EvalFN),
	errorfile_name(Username, ErrorFN),
	concat_atom([
	  '(let ()',
	  ' (defvar *evalfile-name* "', EvalFN, '")',
	  ' (defvar *errorfile* (open "', ErrorFN, '" ',
	  '  :direction :output :if-does-not-exist :create))',
          ' (load "', LispFN, '" :verbose nil))'],
	 Expr).


%%% lisp_eval(+Expr, -Result)
% L��t von einem Lisp-Interpreter Expr auswerten.
% Liefert das Resultat des shell-Aufrufs.

lisp_eval(Expr, Result) :-
	name(Expr, Expr1),
	double_quote(Dq),
	backslash(Bs),
	replace_members(Expr1, [[Dq, [Bs, Dq]]], Expr2),
	lisp_interpreter_call(Expr2, Call, []),
	name(Call1, Call),
	shell(Call1, Result).



%%% analyse_errorfile(+Username, +OR1, -OR2, -Reached, -StopFlag)
% Hilfspr�dikat von eval_exercise.

analyse_errorfile(Username, OR, OR1, Reached, Stop) :-
	errorfile_name(Username, ExmErrorFN),
	see(ExmErrorFN),
	line(Line),
	analyse_errorfile_(Line, ExmErrorFN, OR, OR1, Reached, Stop),
	seen.


%%% analyse_errorfile_(+Line, +FileName, +OR1, -OR2, -Reached, -StopFlag)
% Hilfspr�dikat von analyse_errorfile.

analyse_errorfile_([], _, OR, OR, _, false) :- !.
analyse_errorfile_([directives], _, OR,
		[exm_error(directives, Report)| OR],  _, true) :- !,
	lines(Report).
analyse_errorfile_([loading, exemplary], _, OR,
		[exm_error(loading, Report)| OR],  _, true) :- !,
	lines(Report).
analyse_errorfile_([loading, user], _, OR,
		[error(loading, Report)| OR], 0,  true) :- !,
	lines(Report).
analyse_errorfile_([creating, parameter, lists], _, OR,
		[exm_error(parm_lists, Report)| OR],  _, true) :- !,
	lines(Report).
analyse_errorfile_([accessing, equivalence, function], _, OR,
		[exm_error(equivalence, Report)| OR],  _, true) :- !,
	lines(Report).
analyse_errorfile_([calling, exemplary], _, OR,
		[exm_error(function_call, [Parms1, Report])| OR],  _, true) :-
	read_object(Parms, []), !,
	lines([_| Report]),
	reconstruct(all, Parms, Parms1).
analyse_errorfile_([calling, user], _, OR,
		[error(function_call, [Parms1, Report])| OR], 0, true) :-
	read_object(Parms, []), !,
	lines([_| Report]),
	reconstruct(all, Parms, Parms1).
analyse_errorfile_([comparing| _], _, OR,
		[exm_error(comparing, [Result1r, Result2r, Report])| OR],  _, true) :-
	read_object(Result1, []),
	line([]), line([and]),
	read_object(Result2, []), !,
	lines([_| Report]),
	reconstruct(all, Result1, Result1r),
	reconstruct(all, Result2, Result2r).
analyse_errorfile_(_, FileName, OR, [exm_error(corrupt_file, FileName)| OR],  _, true).



%%% analyse_tracefile(+Username, +OR1, -OR2, +Result1, -Result2, -StopFlag)
% Hilfspr�dikat von eval_exercise. Erweitert die Ausgaberepr�sentation Angaben zu Effizienz
% und Korrektheit und berechnet auf dieser Grundlage eine neue Punktzahl, die f�r
% die L�sung zu vergeben ist.

analyse_tracefile(TraceFN, OR,
		[trace(space, SpaceRatio, SpaceEF1),
		 trace(time, TimeRatio, TimeEF1),
		 trace(equivalence, CEs, CEF)| OR],
		Result, Result1, false) :-
	see(TraceFN),
	lines_until(['Counter-examples:'], Lines),
	extract_efficiency_figures(Lines, Run_time, Space, Rest),
	extract_efficiency_figures(Rest, Run_time1, Space1, _),
	get_counter_examples(CEs),
	line(['Correctness:', C]), !,
	seen,
	TimeRatio is ((Run_time1 + 0.1) / (Run_time + 0.1)),
	SpaceRatio is ((Space1 + 1) / (Space + 1)),
	correctness_eval_factor(C, CEF),
	time_eval_factor(TimeRatio, TimeEF),
	(CEF < 1, TimeEF > 1, !, TimeEF1 = 1;
	 TimeEF1 = TimeEF),
	space_eval_factor(SpaceRatio, SpaceEF),
	(CEF < 1, SpaceEF > 1, !, SpaceEF1 = 1;
	 SpaceEF1 = SpaceEF),
	Result1 is Result * CEF * TimeEF1 * SpaceEF1.
analyse_tracefile(TraceFN, OR, [trace(file_corrupt, TraceFN, [])| OR], Result,
		Result, true) :-
	seen.


%%% get_counter_examples_(+Acc, -Counter_examples)
% Hilfspr�dikat von analyse_tracefile.

get_counter_examples(CEs) :-
	get_counter_examples_([], CEs_l),
	list_to_set(CEs_l, CEs).


%%% get_counter_examples_(+Acc, -Counter_examples)
% Hilfspr�dikat von get_counter_examples.

get_counter_examples_(Acc, CEs) :-
	read_object(CE, []),
	reconstruct(all, CE, CE_r),
	line([]), !,
	% Rekursionsabbruch bei einer Zeile nach einem Objekt, auf der nur ein Leerzeichen steht:
	(space(Sp), CE = [[_, _, Sp]| _], CEs = Acc)>+
	% sonst:
	get_counter_examples_([CE_r| Acc], CEs).





