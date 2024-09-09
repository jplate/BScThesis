%%%%%% Analyse von Lisp-Programmen





:- ensure_loaded('auxprd.pl').




%%%% Herausfiltern von (gültigen) Aufgabenmarkierungen



%%% exercise_tags(+Program, +Exemplaries, -List)
% Gibt eine Liste der in einem Programm vorhandenen gültigen Aufgabenmarkierungen zurück.
% Eine Aufgabenmarkierung ist genau dann gültig, wenn sie in einer der Musterlösungen
% auftaucht.
exercise_tags(Program, Exemplaries, List1) :- 
	exerciset_(valid, Program, Exemplaries, [], List),
	reverse(List, List1).


%%% exerciset(+Mode, +Program, +Exemplaries, +Accumulator, -List)
% Hilfsprädikat von exercise_tags

exerciset_(_, [], _, Acc, Acc).

exerciset_(all, [[X| _]| R], Exm, Acc, Result) :- !,
	exerciset_(all, R, Exm, [X| Acc], Result).

exerciset_(valid, [[X| _]| R], Exm, Acc, Result) :-
 	find_tag(X, Exm, _, _), !,
	exerciset_(valid, R, Exm, [X| Acc], Result).

exerciset_(valid, [_| R], Exm, Acc, Result) :-
	exerciset_(valid, R, Exm, Acc, Result).



%%% find_tag(?Tag, +Exemplaries, -Exercise, -FileName)
%% sucht nach einer Aufgabenmarkierung in der Repräsentation der Musterlösungen

find_tag(T, _, _, _) :- 
	nonvar(T), T = 0, !, 
	fail.
find_tag(X, [[Name, _| Program]| _], [X| Exercise_tail], Name) :-
	member([X| Exercise_tail], Program).
find_tag(X, [_| R], Exercise, Name) :- find_tag(X, R, Exercise, Name).




%%%% Rekonstruktion von Programmzeilen aus der Leserepräsentation



%%% reconstruct(+Level, +Lines1, -Lines2)
% Rekonstruiert aus Zeilenrepräsentationen die ursprüngliche Gestalt
% dieser Zeilen, und zwar bis zu einer Tiefe von Level, falls Level eine Zahl ist.

reconstruct(Level, Lines, Lines1) :-
	reconstruct_(Level, Lines, [], Lines1).


%%% reconstruct_(+Level, +Lines1, +Acc, -Lines2)
% Hilfsprädikat von reconstruct.

reconstruct_(_, [], Acc, Result) :- !,
	reverse(Acc, Result).
reconstruct_(Level, [L| Lines], Acc, Result) :-
	reconstruct_line(Level, L, L1),
	name(L1atom, L1),
	reconstruct_(Level, Lines, [L1atom| Acc], Result).


%%% reconstruct_line(+Level, +Line, -List)

reconstruct_line(Level, [_, IL| LT], List) :-
	reconstruct_line_(Level, IL, LT, [], List).


%%% reconstruct_line_(+Level, +CurrentLevel, +Line_tail, +Acc, -List)
% Hilfsprädikat von reconstruct_line. Liefert eine Liste der Zeichencodes, aus denen
% der durch Line_tail repräsentierte Zeilenrest besteht, mitsamt Zeilenumbrüchen.
% Zu Level s. reconstruct.

reconstruct_line_(_, _, [], [C| Acc], List) :-
	newline(C), !, % dieser Fall tritt auf bei Symbolen oder Strings,
			% die einen Zeilenumbruch beinhalten
	reverse([C| Acc], List).
reconstruct_line_(_, _, [], Acc, List) :- !,
	newline(Nl),
	reverse([Nl| Acc], List).
reconstruct_line_(Level, CL, [X| LT], Acc, List) :-
	opening_parenthesis(X), !,
	CL1 is CL + 1,
	reconstruct_line_newAcc(Level, CL, X, Acc, Acc1),
	reconstruct_line_(Level, CL1, LT, Acc1, List).
reconstruct_line_(Level, CL, [X| LT], Acc, List) :-
	closing_parenthesis(X), !,
	CL1 is CL - 1,
	reconstruct_line_newAcc(Level, CL1, X, Acc, Acc1),
	reconstruct_line_(Level, CL1, LT, Acc1, List).
reconstruct_line_(Level, CL, [X| LT], Acc, List) :-
	reconstruct_line_newAcc(Level, CL, X, Acc, Acc1),
	reconstruct_line_(Level, CL, LT, Acc1, List).


%%% reconstruct_line_newAcc(+Level, +CurrentLevel, +Token, +Acc1, -Acc2)
% Hilfsprädikat von reconstruct_line_. Liefert einen neuen Akkumulator.

reconstruct_line_newAcc(Level, CL, _, Acc, [Space| Acc]) :-
	number(Level),
	CL > Level, !,
	space(Space).
reconstruct_line_newAcc(_, _, [_, X, _], Acc, Acc1) :- !,
	name(X, XList),
	rev_append(XList, Acc, Acc1).
reconstruct_line_newAcc(_, _, X, Acc, [X| Acc]).




%%%% Herausfiltern von Objekten aus der Leserepräsentation



%%% get_objects(+Lines, +InitialLimit, +Filter, -Objects, ?Limit, ?ReachedBottom)
% Liefert in Objects in der richtigen Reihenfolge diejenigen von Filter zurück-
% gegebenen Objekte, die sich in den in Lines repräsentierten Zeilen
% auf den Ebenen 0 bis einschl. Level befinden. Filter ist dabei ein zweistelliges
% Prädikat, das zu einem Listenabschnitt irgendein Objekt liefert.
% Solange nicht ReachedBottom=true ist, werden weitere Antworten gegeben,
% in denen Level schrittweise um 1 ansteigt. Für die erste Antwort gilt aber
% Level=InitialLimit.
% Um also nur die Toplevel-Objekte zu erhalten, rufe man get_objects mit Level=0 auf;
% wenn man dagegen alle Objekte will, sollte man das Prädikat mit ReachedBottom=true
% aufrufen.

get_objects([[_, _| LT]| Lines], InitialLimit, Filter, Objects, Limit, RB) :-
        getobjects([LT| Lines], 0, Filter, [], InitialLimit, true,
		Objects_r, Limit, RB),
	reverse(Objects_r, Objects).


%%% getobjects(+Lines, +CurrentLevel, +Filter, +Acc, +CurrentLimit, +ReachedBottom1,
%%%	-Objects, ?Limit, ?ReachedBottom2)
% Hilfsprädikat von get_objects.

% Rekursionsabbruch, falls keine Zeilen vorhanden
getobjects([], _, _, Acc, Limit, RB, Acc, Limit, RB) :- !.

% Rekursionsabbruch, falls letzte Zeile leer und keine weiteren vorhanden
getobjects([[]], _, _, Acc, Limit, RB, Acc, Limit, RB) :- !.

% Übergang zur nächsten Zeile
getobjects([[], [_, _| LT]| Lines], Level, Filter, Acc, Limit, RB,
		Result, Limit1, RB1) :- !,
	getobjects([LT| Lines], Level, Filter, Acc, Limit, RB, Result, Limit1, RB1).

% Wenn Level noch nicht Limit erreicht hat und X Filter erfüllt, X in die Ergebnisliste
% aufnehmen
getobjects([[X| LT]| Lines], Level, Filter, Acc, Limit, RB, Result, Limit1, RB1) :-
	Level =< Limit,
	Goal =.. [Filter, [X| LT], X1],
	Goal, !,
	getobjects_([[X| LT]| Lines], Level, Filter, [X1| Acc], Limit, RB, Result, Limit1, RB1).

% sonst weiter mit getobjects_
getobjects([LT| Lines], Level, Filter, Acc, Limit, RB, Result, Limit1, RB1) :-
	getobjects_([LT| Lines],  Level, Filter, Acc, Limit, RB, Result, Limit1, RB1).


%%% getobjects_(+Lines, +CurrentLevel, +Filter, +Acc, +CurrentLimit, +ReachedBottom1,
%%%	-Objects, ?Limit, ?ReachedBottom2)
% Hilfsprädikat von getobjects. Verantwortlich für die Einhaltung von Beschränkungen
% bezgl. Klammerungsebenen

% Bei öffnender Klammer, falls Level Limit erreicht, Level erhöhen und RB auf false setzen
getobjects_([[X| LT]| Lines], Level, Filter, Acc, Limit, _, Result, Limit1, false) :-
	opening_parenthesis(X),
	Level >= Limit,
	Level1 is Level + 1,
	getobjects([LT| Lines], Level1, Filter, Acc, Limit, false, Result, Limit1, _).

% Alternative, falls Level=Limit und RB=true (Limit also noch nicht überschritten wurde):
% auch Limit erhöhen, RB aber nicht verändern
getobjects_([[X| LT]| Lines], Level, Filter, Acc, Limit, true, Result, Limit2, RB1) :-
	opening_parenthesis(X),
	Level = Limit, 
	(var(Limit2); Limit < Limit2), !,
	Limit1 is Limit + 1,
	Level1 is Level + 1,
	getobjects([LT| Lines], Level1, Filter, Acc, Limit1, true, Result, Limit2, RB1).

% Bei öffnender Klammer: wenn Level < Limit, Level erhöhen
getobjects_([[X| LT]| Lines], Level, Filter, Acc, Limit, RB, Result, Limit1, RB1) :-
	opening_parenthesis(X), !,
	Level < Limit,
	Level1 is Level + 1,
	getobjects([LT| Lines], Level1, Filter, Acc, Limit, RB, Result, Limit1, RB1).

% Bei schließender Klammer Level verringern
getobjects_([[X| LT]| Lines], Level, Filter, Acc, Limit, RB, Result, Limit1, RB1) :-
	closing_parenthesis(X), !,
	Level1 is Level - 1,
	getobjects([LT| Lines], Level1, Filter, Acc, Limit, RB, Result, Limit1, RB1).

% Wenn Level noch nicht Limit erreicht hat und X Filter erfüllt, X in die Ergebnisliste
% aufnehmen
getobjects_([[X| LT]| Lines], Level, Filter, Acc, Limit, RB, Result, Limit1, RB1) :-
	Level =< Limit,
	Goal =.. [Filter, [X| LT], X1],
	Goal, !,
	getobjects([LT| Lines], Level, Filter, [X1| Acc], Limit, RB, Result, Limit1, RB1).

% sonst weiter mit getobjects
getobjects_([[_| LT]| Lines], Level, Filter, Acc, Limit, RB, Result, Limit1, RB1) :-
	getobjects([LT| Lines],  Level, Filter, Acc, Limit, RB, Result, Limit1, RB1).



%%%% Berechnung von Ergebnis-Punktzahlen


%%% get_result(+HintInfo, +Result_without_hints, -Result)

get_result([], Acc, Acc) :-  !.	
get_result([[_, EF]| R], Acc, Result) :-
	Acc1 is EF * Acc,
	get_result(R, Acc1, Result).




%%% Sonstiges


%%% trim_line_list(+Lines1, -Lines2)
% Liefert in Lines2 eine Kopie von Lines1 zurück, aus der
% alle führenden und folgenden Leerzeilen entfernt sind.

trim_line_list(Lines, Lines2) :-
	trimlinelist(Lines, false, [], Lines1r),
	trimlinelist(Lines1r, false, [], Lines2).


%%% trimlinelist(+Lines1, +Non_empty_line_encountered, +Acc, -Lines2)
% Hilfsprädikat von trim_line_list.

trimlinelist([], _, Acc, Acc) :- !.
trimlinelist([[_, _| End]| R], false, Acc, Result) :-
	(End = []; End = [[end| _]]), !,
	trimlinelist(R, false, Acc, Result).
trimlinelist([X| R], _, Acc, Result) :-
	trimlinelist(R, true, [X| Acc], Result).



