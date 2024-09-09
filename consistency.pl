%%%%%% Überprüfung von Layout und Klammerung





:- ensure_loaded('auxprd.pl').

% Der hier implementierte Algorithmus überprüft eine Leserepräsentation auf Konsistenz
% zwischen Layout und Klammerung (damit auch auf reine Klammerungsfehler) und gibt ggf.,
% in Form einer speziellen Ausgaberepräsentation, eine Diagnose zurück.





%%%%% Erste Ebene: check_consistency
%
% Herstellen der Prä-Konsistenzrepräsentation aufgrund der Leserepräsentation;
% diese mit checkconsis analysieren; die ursprüngliche mit der modifizierten
% Prä-Konsistenzrepräsentation vergleichen; Synthese der Ausgaberepräsentation.
%
%%% check_consistency(+Read_Representation, -Output_Representation)

check_consistency([_, _| RRT], OR4) :-
	pre_consistency(RRT, PrCR),
	checkconsis(PrCR, [], PrCR1, OR1),
	compare_indentations(PrCR, PrCR1, OR2),
	rev_append(OR2, OR1, OR3),
	predsort(or_compare, OR3, OR4).



%%% or_compare(OR_element1, OR_element2)
% Determiniert die Reihenfolge, in der die Elemente der Ausgabe-Repräsentation
% dargestellt werden.


%% Fälle, in denen OR_element1 vor OR_element2 einzuordnen ist

% Keine Überlappung
or_compare(<, [_, No2| _], [No3| _]) :-
	No2 =< No3, !.

% Strenge Inklusion: äußere Elemente kommen vor inneren
or_compare(<, [No1, No2| _], [No3, No4| _]) :-
	No1 < No3,
	No2 > No4, !.

% Andere Überlappungen
or_compare(<, [No1, No2| _], [No3, No4| _]) :-
	No1 =< No3,
	No2 < No4, !.
or_compare(<, [No1, No2| _], [No3, No4| _]) :-
	No1 < No3,
	No2 =< No4.


%% Andere Fälle

% Äquivalenz
or_compare(=, [No1, No2| _], [No1, No2| _]) :- !.

% OR_element1 ist nach OR_element2 einzuordnen
or_compare(>, _, _).



%%% compare_indentations(+PrCR1, +PrCR2, -OR)
% Hilfsprädikat von check_consis.
% Das Prädikat vergleicht PrCRT1 und PrCRT2 miteinander und erzeugt daraus eine
% Liste der Einrückungs-Differenzen, die Output-Repräsentation für check_consis.

compare_indentations(PrCR1, PrCR2, OR) :-
	compareindent(PrCR1, PrCR2, [], OR).


%%% compareindent(+PrCR1, +PrCR2, +Acc, -OR)
% Hilfsprädikat von compare_indentations.

compareindent([], [], Acc, Acc).
compareindent([[No, 0, Indent1| _]| PrCRT1], [[No, 0, Indent2| _]| PrCRT2], Acc, OR) :-
	Delta is Indent2 - Indent1,
	Delta \= 0, !,
	compareindent(PrCRT1, PrCRT2, [[No, No, indentation, Delta]| Acc], OR).
compareindent([_| PrCRT1], [_| PrCRT2], Acc, OR) :-
	compareindent(PrCRT1, PrCRT2, Acc, OR).




%%%% Erzeugung der Prä-Konsistenzrepräsentation
%
% Eine Prä-Konsistenzrepräsentation hat folgende Form:
%
% [[LineNo, Start, Indentation, LayoutLevel,
%	Starting_with_closing_parenthesis| Characters] ...]
%



%%% pre_consistency(+Read_representation_tail, -Pre_consistency_representation)
% Nimmt den Aufgabenteil einer Leserepräsentation und liefert die entsprechende
% Prä-Konsistenzrepräsentation (die Vorstufe der Konsistenzrepräsentation).

pre_consistency(RRT, [Start_line| PrCR]) :-
	Start_line = [0, 0, 0, 0, false],
	precons(RRT, [], PrCR).


%%% precons(+Exercises, +Acc, -Pre_consistency_representation)
% Hilfsprädikat von pre_consistency. Bearbeitet den Aufgabenteil der Leserepräsentation
% und übergibt für die Bearbeitung der einzelnen Aufgaben an precons_.

% Rekursionsabbruch bei leerem Programm
precons([], [_], []) :- !.

% Rekursionsabbruch bei nicht leerem Programm; Anfügen einer Endzeilenrepräsentation (die
% Repräsentation der von read_file gelieferten Endzeile enthält möglicherweise eine zu hohe
% Zeilennummer)
precons([], [[_, _, _, LL| _], [No| Line]| Acc], Result) :- !,
	No1 is No + 1,
	reverse([[No1, 0, 0, LL, false], [No| Line]| Acc], Result).

precons([[_, _, ET]| RRT], Acc, Result) :-
	precons_(ET, Acc, Acc1),
	precons(RRT, Acc1, Result).


%%% precons_(+Exercise_tail, +Acc, -Lines)
% Hilfsprädikat von precons. Liefert für jede Aufgabe der Leserepräsentation
% die entsprechende Prä-Konsistenzrepräsentation.
% Das Prädikat geht davon aus, daß Zeilen, die außer Leerzeichen nichts enthalten, so
% repräsentiert sind, als ob sie gar nichts enthielten.

% Rekursionsabbruch
precons_([], Acc, Acc).

% Ignorieren von Zeilen, die nichts enthalten außer Leerzeichen
precons_([[_, _]| ET], Acc, Lines) :- !,
	precons_(ET, Acc, Lines).

% Ignorieren von Zeilen, die in einem String beginnen
precons_([[_, _, [string| _]| _]| ET], Acc, Lines) :- !,
	precons_(ET, Acc, Lines).

% Ignorieren von Zeilen, die in einem von '|' begrenzten Symbol beginnen
precons_([[_, _, [symbol| _]| _]| ET], Acc, Lines) :- !,
	precons_(ET, Acc, Lines).

% Erzeugung der Repräsentationen der anderen Zeilen
precons_([[No, IL| LT]| ET], Acc, Lines) :-
	line_info(IL, LT, 0, LL, StClP, ClPCols, OpPCols),
	indent([No, IL| LT], Indent),
	prc_insert_line([No, 0, Indent, LL, StClP| LT], IL, ClPCols, OpPCols, Acc, Acc1),
	precons_(ET, Acc1, Lines).


%%% prc_insert_line(+Line, +Number_of_closing_parentheses, +Initial_level,
%%% 	+Column_numbers_of_opened_parentheses, +Acc1, -Acc2)
% Hilfsprädikat von precons_.

prc_insert_line([No, Start, Indent, LL, StClP| LT], IL, ClPCols, [OpPCol1| _], Acc,
		[[No1, OpPCol1, Indent, LL1, false| LT2],
	 	 [No, Start, Indent, LL, StClP| LT1]| Acc]) :-
	length(ClPCols, Ln),
	Ln > 0, !,
	divide_list_at(OpPCol1, LT, LT1, LT2),
	LL1 is IL - Ln,
	No1 is No + 0.1.
prc_insert_line(Line, _, _, _, Acc, [Line| Acc]).


%%% line_info(+Initial_level, +Line_tail, +Char_position, -Layout_level,
%%% 	-Starting_with_closing_parenthesis, -Closed_parentheses_column_numbers,
%%%	-Opened_parentheses_column_numbers)
% Liefert die Layout-Ebene einer Zeile, ob diese mit einer schließenden Klammer als
% erstem Nicht-Leerzeichen beginnt, und eine Liste mit den Spaltennummern
% der öffnenden, aber nicht auf derselben Zeile wieder geschlossenen Klammern.
% Die Layout-Ebene einer Zeile bestimmt, wie weit
% diese eingerückt sein muß. Wenn eine Zeile mit einer schließenden Klammer beginnt, ist
% ihre Layout-Ebene nicht gleich der Anfangs-Ebene, sondern um soviel niedriger, wieviele
% auf vorhergehenden Zeilen beginnenden Konstrukte sie abschließt. Z.B. ist die Layout-Ebene
% einer nur aus ")))" oder auch "))) (x) (" bestehenden Zeile ihre Anfangsebene minus 3.
% Zur Erläuterung, warum dies sinnvoll ist, s. Einleitungskommentar zu check_layout.

% Leerzeichen überspringen
line_info(IL, [C| LT], CP, LL, StClP, ClPCols, OpPCols) :-
	space(C), !,
	CP1 is CP + 1,
	line_info(IL, LT, CP1, LL, StClP, ClPCols, OpPCols).

% Bei schließender Klammer an line_info_ übergeben
line_info(IL, [C| LT], CP, LL, true, ClPCols, OpPCols) :-
	closing_parenthesis(C), !,
	Level is IL-1,
	CP1 is CP + 1,
	line_info_(Level, LT, CP1, [CP], [], ClPCols, OpPCols, LL).

% Bei allen anderen Zeichen ist die Layout-Ebene gleich der Anfangs-Ebene
line_info(L, [C| LT], CP, L, false, ClPCols, OpPCols) :-
	line_info_(L, [C| LT], CP, [], [], ClPCols, OpPCols, _).


%%% line_info_(+Current_level, +Line_tail, +Char_Position, +Acc, +Stack,
%%%	-Closed_parentheses_column_numbers, -Opened_parentheses_column_numbers, -Layout_level)
% Hilfsprädikat zu line_info.
% Bestimmt die Layout-Ebene, wenn das erste Nicht-Leerzeichen einer Zeile eine schließende
% Klammer ist, sowie (in Closed_parentheses_column_numbers) die Spaltennummern der in dieser
% Zeile geschlossenen, aber nicht geöffneten und (in Opened_parentheses_column_numbers)
% die Spaltennummern der in dieser Zeile geöffneten, aber nicht geschlossenen Klammern.

% Rekursionsabbruch am Ende der Zeile
line_info_(Level, [], _, Acc, Stack, ClPCols, OpPCols, Level) :- !,
	reverse(Acc, ClPCols),
	reverse(Stack, OpPCols).

% Ebene dekrementieren bei einer schließenden Klammer, die nicht zu einer
% öffnenden Klammer auf derselben Zeile gehört (Stack der öffnenden Klammern ist leer)
line_info_(Level, [C| LT], CP, Acc, [], ClPCols, OpPs, LL) :-
	closing_parenthesis(C), !,
	CP1 is CP + 1,
	Level1 is Level-1,
	line_info_(Level1, LT, CP1, [CP1| Acc], [], ClPCols, OpPs, LL).

% Sonst bei schließender Klammer den Stack der öffnenden Klammern verkleinern
line_info_(Level, [C| LT], CP, Acc, [_| Stack], ClPCols, OpPs, LL) :-
	closing_parenthesis(C), !,
	CP1 is CP+1,
	line_info_(Level, LT, CP1, Acc, Stack, ClPCols, OpPs, LL).

% Bei öffnender Klammer diesen Stack entsprechend vergrößern
line_info_(Level, [C| LT], CP, Acc, Stack, ClPCols, OpPs, LL) :-
	opening_parenthesis(C), !,
	CP1 is CP+1,
	line_info_(Level, LT, CP1, Acc, [CP1| Stack], ClPCols, OpPs, LL).

% Ignorieren aller anderen Zeichen
line_info_(Level, [_| LT], CP, Acc, Stack, ClPCols, OpPs, LL) :-
	CP1 is CP+1,
	line_info_(Level, LT, CP1, Acc, Stack, ClPCols, OpPs, LL).



%%% indent(+Line, -N)
% Liefert die Anzahl der Leerzeichen, die eine Zeile eingerückt ist (Tabs wurden bereits
% zuvor von read_line in Leerzeichen übersetzt).

indent([_, _| LT], N) :-
	indent_(LT, 0, N).


%%% indent_(+Line, +Acc, -Result)
% Hilfsprädikat von indent.

indent_([C| LT], N, Result) :-
	space(C), !,
	N1 is N+1,
	indent_(LT, N1, Result).
indent_(_, N, N).





%%%%% Zweite Ebene: checkconsis
%
% Erzeugen der Konsistenzrepräsentation durch consistency; die hierin gespeicherten
% Inkonsistenzen durch checkconsis_ auf Klammerungs- oder Einrückungsfehler zurück-
% führen und die Prä-Konsistenzrepräsentation entsprechend modifizieren; mit dem
% Ergebnis in die Rekursion gehen, falls Modifikationen vorgenommen wurden.
%
%%% checkconsis(+PrCR1, +OR1, -PrCR2, -OR2)
% Hilfsprädikat von check_consistency.

checkconsis(PrCR, OR, PrCR2, OR2) :-
	trim_stacks,
	consistency(PrCR, CR),

	/*
	findall([LineNo, Start, Indent, LL], member([LineNo, Start, Indent, LL| _], PrCR), P1),
	nl, write(P1), nl,
	extri(CR, CR_), write(CR_), nl,
	*/

	(CR = [], PrCR2 = PrCR, OR2 = OR)>+
	reverse(PrCR, PrCRr), !,
	checkconsis_(CR, 0, end, none, PrCR, PrCRr, OR, PrCR1, OR1, _),
	checkconsis(PrCR1, OR1, PrCR2, OR2).




%%%% Erzeugung der Konsistenzrepräsentation



%%% consistency(+Pre_Consistency_Representation, -Consistency_Representation)
% Liefert zu einer Prä-Konsistenzrepräsentation die entsprechende Konsistenzrepräsentation.

consistency([Line| PrCRT], CR) :-
	inconsistencies([[[Line], [Line| PrCRT]]], [], Is),
	predsort(i_compare, Is, CR).



%%% i_compare(Delta, Inconsistency1, Inconsistency2)
% Determiniert die Reihenfolge, in der die Inkonsistenzen in der Konsistenzrepräsentation
% dargestellt werden (wichtig für check_parentheses).


%% Fälle, in denen Inconsistency1 vor Inconsistency2 einzuordnen ist:

% Keine Überlappung
i_compare(<, [_, [No2| _]| _], [[No3| _]| _]) :-
	No2 =< No3, !.

% Inklusion: äußere Elemente kommen vor inneren
i_compare(<, [[No1| _], [No2| _]| _], [[No3| _], [No4| _]| _]) :-
	No1 < No3,
	No2 >= No4, !.
i_compare(<, [[No1| _], [No2| _]| _], [[No3| _], [No4| _]| _]) :-
	No1 =< No3,
	No2 > No4, !.

% Andere Überlappungen (sollten eigentlich nicht auftauchen)
i_compare(<, [[No1| _], [No2| _]| _], [[No3| _], [No4| _]| _]) :-
	No1 < No3,
	No2 < No4.


%% Andere Fälle

% Äquivalenz
i_compare(=, [[No1| _], [No2| _]| _], [[No1| _], [No2| _]| _]) :- !.

% Inconsistency1 ist nach Inconsistency2 einzuordnen
i_compare(>, _, _).



%%% inconsistencies(+Agenda, +Acc, -Inconsistencies_reversed)
% Hilfsprädikat von consistency. Liefert (sofern Acc=[]) eine Liste der
% in Pre_Consistency_Representation auftretenden Inkonsistenzen, also die entsprechende
% "Konsistenzrepräsentation".
% Funktionsweise: Der Vergleichsgraph für die Inkonsistenzprüfungen wird mit Hilfe einer
% Agenda depth-first-mäßig traversiert. Um eine mehrfache Expansion derselben Zeilen
% (die ja die Knoten dieses  Graphen bilden) zu vermeiden, wird die Einrückungsweite
% einer möglicherweise zu expandierenden Zeile mit der ihrer Mutterzeile verglichen.
% Wenn sie weniger weit eingerückt ist als diese, wird sie nicht mehr expandiert, denn
% dann ist sie bereits Nachfolgerin einer noch früheren Zeile.
% Die Nachfolgerinnen einer Zeile liefert (implizit) successors.
% Elemente der Agenda haben die Form [Tail1, Tail2], wobei Tail1 und Tail2
% Restlisten der Prä-Konsistenzrepräsentation sind. Die jeweils ersten Elemente dieser Listen
% werden in einem gegebenen Aufruf von inconsistencies auf eventuell zwischen ihnen bestehende
% Inkonsistenz hin verglichen (das erste Element von Tail2 ist Nachfolgerin des ersten
% Elements von Tail1). In diesem Vergleich und ggf. dem Eintrag der Repräsentation einer
% gefundenen Inkonsistenz in Acc besteht das "Abarbeiten" eines Elements der Agenda.
% Die Agenda speichert deshalb Restlisten der Prä-Konsistenzrepräsentation anstelle
% der bloßen Zeilen, weil es so einfacher ist, auf die Nachfolgerinnen der einzelnen Zeilen
% zuzugreifen.
% Dies ist auch der Grund, warum successors keine Zeilen, sondern diejenigen Restlisten der
% Prä-Konsistenzrepräsentation liefert, die die Nachfolgerzeilen als erste Elemente
% enthalten.

% Rekursionsabbruch bei leerer Agenda
inconsistencies([], Acc, Acc) :- !.

% Inkonsistenz eintragen, Agenda erweitern
inconsistencies([[PrCRT1, PrCRT2]| Agenda], Acc, Result) :-
	[PrCRT1, PrCRT2] = [[[_, _, Indent1| _]| _], [[_, _, Indent2| _]| PrCRT2_]],
	Indent1=<Indent2,
	inconsistency(PrCRT1, PrCRT2, I), !,
	successors_(Indent2, PrCRT2_, [], Successors),
	new_agenda(PrCRT2, Successors, Agenda, New_agenda),
	inconsistencies(New_agenda, [I| Acc], Result).

% Inkonsistenz eintragen; keine Erweiterung der Agenda
inconsistencies([[PrCRT1, PrCRT2]| Agenda], Acc, Result) :-
	inconsistency(PrCRT1, PrCRT2, I), !,
	inconsistencies(Agenda, [I| Acc], Result).

% Keine Inkonsistenz; Agenda erweitern
inconsistencies([[PrCRT1, PrCRT2]| Agenda], Acc, Result) :-
	[PrCRT1, PrCRT2] = [[[_, _, Indent1| _]| _], [[_, _, Indent2| _]| PrCRT2_]],
	Indent1=<Indent2, !,
	successors_(Indent2, PrCRT2_, [], Successors),
	new_agenda(PrCRT2, Successors, Agenda, New_agenda),
	inconsistencies(New_agenda, Acc, Result).

% Keine Inkonsistenz; keine Erweiterung der Agenda
inconsistencies([_| Agenda], Acc, Result) :-
	inconsistencies(Agenda, Acc, Result).


%%% new_agenda(+Tail, +Tails, +Old_agenda, -New_agenda)
% Liefert eine neue (erweiterte) Agenda für inconsistencies.

new_agenda(_, [], Agenda, Agenda) :- !.
new_agenda(Tail1, [Tail2| Tails], Agenda, New_agenda) :-
	new_agenda(Tail1, Tails, [[Tail1, Tail2]| Agenda], New_agenda).


%%% inconsistency(+PrCR_tail1, +PrCR_tail2, -Inconsistency)
% Liefert eine Repräsentation der Inkonsistenz zwischen der ersten Zeile in
% PrCR_tail1 und der ersten Zeile in PrCR_tail2, wenn eine solche besteht.

inconsistency([Line1| _], [Line2| _], [Line1, Line2, Type, Degree]) :-
	inconsistent(Line1, Line2, Type, Degree).


%%% inconsistent(+Line1, +Line2, -Type, -Degree)
% Trifft zu, wenn zwischen Line1 und Line2 eine Inkonsistenz besteht. Gibt in dem Fall
% Typ und Grad dieser Inkonsistenz zurück.
% Es wird angenommen, daß die Nummer von Line1 niedriger ist als die von Line2.
% Wenn das Prädikat auf Line1 und Line2 zutrifft, werden Line1 und Line2 auch als
% "Anfangs-" bzw. "Endzeile" der zwischen ihnen bestehenden Inkonsistenz bezeichnet.

inconsistent([_, _, Indent1, LL1| _], [_, _, Indent2, LL2| _], -1, Degree) :-
	Indent1<Indent2, LL1>=LL2, !,
	Degree is LL1-LL2+1.

inconsistent([_, _, Indent1, LL1| _], [_, _, Indent2, LL2| _], +1, Degree) :-
	Indent1>Indent2, LL1=<LL2, !,
	Degree is LL2-LL1+1.

inconsistent([_, _, Indent1, LL1| _], [_, _, Indent2, LL2| _], -1, Degree) :-
	Indent1=Indent2, LL1>LL2, !,
	Degree is LL1-LL2.

inconsistent([_, _, Indent1, LL1| _], [_, _, Indent2, LL2| _], +1, Degree) :-
	Indent1=Indent2, LL1<LL2, !,
	Degree is LL2-LL1.



%%% successors(+Indentation, +Pre_Consistency_Representation_Tail, -Tails)
% Liefert (in richtiger Reihenfolge) diejenigen Restlisten der Prä-Konsistenz-
% Repräsentation, deren jeweils erste Elemente die "Nachfolger" N(z) derjenigen Zeile z sind,
% die in der Prä-Konsistenz-Repräsentation direkt vor Pre_Consistency_Representation_Tail
% kommt. Indentation sollte die Einrückungsweite dieser Zeile sein.

successors(Indent, PrCRT, Tails1) :-
	successors_(Indent, PrCRT, [], Tails),
	reverse(Tails, Tails1).


%%% successors_(+Indentation, +Pre_Consistency_Representation_Tail, +Acc, -Tails)
% Hilfsprädikat von successors.
% Kann direkt benutzt werden, um die umgedrehte Liste der N(z) zu erhalten.

successors_(_, [], Acc, Acc) :- !.

successors_(Indent, [[No1, Start1, Indent1| LT1]| PrCRT], Acc,
		[[[No1, Start1, Indent1| LT1]| PrCRT]| Acc]) :-
	Indent1=<Indent, !.

successors_(Indent, [[No1, Start1, Indent1| LT1]| PrCRT], Acc, Result) :-
	 next_successor(PrCRT, Indent1, PrCRT1),
	 successors_(Indent, PrCRT1, [[[No1, Start1, Indent1| LT1]| PrCRT]| Acc], Result).


%%% next_successor(+Pre_Consistency_Representation_Tail, +Indentation,
%%%	-Pre_Consistency_Representation_Tail)
% Überspringt alle Zeilenrepräsentationen in Pre_Consistency_Representation_Tail, die weiter
% oder gleich weit eingerückt sind als/wie Indentation.

next_successor([], _, []) :- !.

next_successor([[No1, Start1, Indent1| LT1]| PrCRT], Indent,
		[[No1, Start1, Indent1| LT1]| PrCRT]) :-
	Indent1<Indent, !.

next_successor([_| PrCRT], Indent, Rest) :-
	next_successor(PrCRT, Indent, Rest).






%%%%% Dritte Ebene: checkconsis_ und repair_inconsistency
%
% Jede in der übergebenen Konsistenzrepräsentation enthaltene Inkonsistenz
% entweder auf Einrückungs- oder Klammerungsfehler zurückführen und die
% entsprechenden Modifikationen an der Prä-Konsistenzrepräsentation durchführen.
%
%%% checkconsis_(+CR_tail, +Start_line_no, +End_line_no, +Global_inconsistency1,
%%% 	+PrCR1, +PrCR1_reversed, +OR1, -PrCRT2, -OR2, -Global_inconsistency2)
% Hilfsprädikat von checkconsis.
% Traversiert die Konsistenzrepräsentation wie einen Baum, d.h. das Prädikat behandelt auf
% der obersten Rekursionsebene nur die äußersten Inkonsistenzen und ruft sich für die
% von diesen umspannten Inkonsistenzen rekursiv auf.

checkconsis_([], _, _, GlobalI, PrCR, _, OR, PrCR, OR, GlobalI) :- !.

checkconsis_([[_, [No| _]| _]| _], _, End, GlobalI, PrCR, _, OR,
		PrCR, OR, GlobalI) :-
	number(End),
	End < No, !.

checkconsis_([[[No| _], _| _]| CRT], Start, End, GlobalI, PrCR, PrCRr, OR,
		PrCR1, OR1, GlobalI1) :-
	Start > No, !,
	checkconsis_(CRT, Start, End, GlobalI, PrCR, PrCRr, OR,
		PrCR1, OR1, GlobalI1).

checkconsis_([I| CRT], _, End, GlobalI, PrCR, PrCRr, OR,
		PrCR3, OR3, GlobalI4) :-
	I = [[No1| _], [No2| _], _, _], !,
	global_inconsistency(I, GlobalI, GlobalI1),
	checkconsis_(CRT, No1, No2, GlobalI1, PrCR, PrCRr, OR,
		PrCR1, OR1, GlobalI2),
	reverse(PrCR1, PrCR1r),
	repair_inconsistency([I| CRT], GlobalI2, PrCR1, PrCR1r, OR1,
		GlobalI3, PrCR2, OR2), !,
	reverse(PrCR2, PrCR2r),
	checkconsis_(CRT, No2, End, GlobalI3, PrCR2, PrCR2r, OR2,
		PrCR3, OR3, GlobalI4).


%%% global_inconsistency(+Current_inconsistency, +Current_global_inconsistency,
%%%	-New_global_inconsistency)
% Hilfsprädikat von checkconsis_.
% Liefert zu einer Inkonsistenz die sie umspannende "globale" Inkonsistenz.
% "Global" heißt eine Inkonsistenz dann, wenn ihre erste Zeile nicht eingerückt ist.
% Das Prädikat liefert das Atom none, wenn es eine solche Inkonsistenz nicht gibt.

global_inconsistency([[No1, St1, 0| LT1], Line2, Type, Degree], _,
	[[No1, St1, 0| LT1], Line2, Type, Degree]) :- !.
global_inconsistency([[No1C| _], _, _, _], [_, [No2G| _], _, _], none) :-
	No1C >= No2G, !.
global_inconsistency(_, GlobalI, GlobalI).



%%% repair_inconsistency(+CRT1, +Global_inconsistency1, +PrCR1, +PrCRr, +OR1,
%%%	-Global_inconsistency2, -PrCR2, -OR2)
% Hilfsprädikat von checkconsis_. Stellt zuerst fest, inwieweit die aktuelle
% Inkonsistenz noch existiert, bevor es an repair_ übergibt.

repair_inconsistency([I| CRT], GlobalI, PrCR, PrCRr, OR, GlobalI1, PrCR1, OR1) :-
	I = [[No1| _], [No2| _], _, _],

	% Herausfinden, inwiefern die Inkonsistenz noch Bestand hat
	first_member([No1| LT1], PrCR, PrCRT1),
	first_member([No2| LT2], PrCRT1),
	inconsistency([[No1| LT1]| _], [[No2| LT2]| _], I1), !,

	repair([I1| CRT], GlobalI, PrCR, PrCRr, OR, GlobalI1, PrCR1, OR1),

	/*
	nl, write(I1), nl,
	write(PrCR1), nl,
	write(OR1), nl,
	*/
	true.


% Inkonsistenz ignorieren, falls sie in der aktuellen PrCR nicht mehr existiert
repair_inconsistency(_, GlobalI, PrCR, _, OR, GlobalI, PrCR, OR).


%%% repair(+CRT1, +Global_inconsistency1, +PrCR1,
%%%	+PrCRr, +OR1, -Global_inconsistency2, -PrCR2, -OR2)
% Hilfsprädikat von repair_inconsistency.
% Versucht, ein Inkonsistenzpaar (1. Klausel) oder einzelne Inkonsistenzen (2. Klausel)
% auf Einrückungsfehler zurückzuführen, oder eine einzelne Inkonsistenz auf einen
% Klammerungsfehler (3. Klausel), und gibt ggf. eine veränderte
% Prä-Konsistenzrepräsentation zurück.

% Inkonsistenzpaare: Einrückungsfehler?
repair([I| CRT], GlobalI, PrCR, PrCRr, OR, GlobalI, PrCR1, OR) :-
	I = [Line1, [No2| LT2], Type_I, Deg_I],

	% Nach einer Partnerinkonsistenz suchen und sicherstellen, daß diese noch existiert
	member([[No3| _], [No4| _], Type_J, _], CRT),
	No3 > No2 ->
	Type_J =:= -Type_I,
	first_member([No3| LT3], PrCR, PrCRT1),
	first_member([No4| LT4], PrCRT1),
	inconsistent([No3| LT3], [No4| LT4], Type_J, Deg_J1),

	% Modifikation der Prä-Konsistenzrepräsentation vorbereiten
	Deg_I1 is min(Deg_J1, Deg_I),
	I1 = [Line1, [No2| LT2], Type_I, Deg_I1],
	delta_and_line(I1, Delta, [No2| LT2]),
	Delta \= 0,

	% Prä-Konsistenzrepräsentation modifizieren
	modify_indent(No2, Delta, PrCR, PrCRr, PrCR1), !.

% Einzelinkonsistenzen: Einrückungsfehler?
repair([I| _], GlobalI, PrCR, PrCRr, OR, GlobalI, PrCR1, OR) :-
	I = [_, _, TypeI, DegI],

	% Entweder es gibt keine globale Inkonsistenz, die I umspannt, oder sie ist nicht
	% vom selben Typ, oder I ist stärker als die globale.
	(GlobalI = none,
	 I1 = I;
	 GlobalI = [_, _, TypeG, DegG],
	 (TypeG =\= TypeI,
	  I1 = I;
	  DegI > DegG,
	  weaken(I, DegG, I1))),

	% Prä-Konsistenzrepräsentation modifizieren
	delta_and_line(I1, Delta, [No| _]),
	Delta \= 0,
	modify_indent(No, Delta, PrCR, PrCRr, PrCR1), !.

% Falls nicht Einrückungsfehler: Klammerungsfehler
repair([I| _], GlobalI, PrCR, _, OR, GlobalI1, PrCR1,
		[[No1t, No2t, parentheses, Start1, Type, Degree, Suspects]| OR]) :-
	I = [[No1| LT1], [No2| LT2], Type, Degree],

	% Finden und Filtern der 'verdächtigen' Zeilen
	first_member([No1| _], PrCR, PrCRT1),
	first_member([No2| _], PrCRT1, PrCRT2),
	findall(PrCRT,
		suspect_PrCRT([[No1| LT1]| PrCRT1], [[No2| LT2]| PrCRT2], Type, PrCRT),
		SuspectPrCRTs),
	filter_suspects(I, PrCR, SuspectPrCRTs, SuspectPrCRTs1),

	% Modifikation der PrCR und der globalen Inkonsistenz
	modify_ll(PrCR, SuspectPrCRTs1, Type, Degree, PrCR1),
	(GlobalI = [_, _, Type, _], !,
	 weaken(GlobalI, Degree, GlobalI1);
	 GlobalI1 = GlobalI),

	% Vorbereitung der Ausgaberepräsentation
	LT1 = [Start1| _],
	No1t is truncate(No1),
	No2t is truncate(No2),
	findall([No_t, End],
		(member([[No|_]| PrCRT], SuspectPrCRTs1),
		 No_t is truncate(No),
		 get_end_column(PrCRT, End)),
		Suspects).





%%%%% Vierte Ebene: Hilfsprädikate von repair




%%%% Behandlung von Einrückungsfehlern



%%% modify_indent(+No, +Delta, +PrCR1, +PrCR1reversed, -PrCR2)
% Sammelt zunächst alle Zeilen, an die Zeile Nr. No
% Type-einrückungsgebunden ist und läßt dann den Einrückungsmodifikation der betreffenden
% Zeilen von modifyindent um Delta ändern (sofern diese Modifikation von delta_test erlaubt wird).
% Beweis schlägt fehl, wenn die Modifikation nicht erlaubt ist.

modify_indent(No, Delta1, PrCR, PrCRr, PrCR1) :-
	findall([B| Delta], indentation_binder(Delta1, No, _, PrCR, PrCRr, [B| _], Delta),
		BDeltaPairs),
	margin(M),
	forall(member([Line| Delta], BDeltaPairs),
	 	delta_test(Line, Delta, M)),
	modifyindent(PrCR, BDeltaPairs, [], PrCR1).


%%% modifyindent(+PrCR1, +BinderDeltaPairs, +Acc, -PrCR2)
% Hilfsprädikat von modify_indent. Besorgt die Einrückung der Zeilen in List um Delta und
% bildet so aus PrCR1 eine neue Prä-Konsistenzrepräsentation PrCR2.

modifyindent([], _, Acc, Result) :- !,
	reverse(Acc, Result).
modifyindent([[No, Start, Indent, LL| LT]| PrCRT], Pairs, Acc, Result) :-
	member([[No| _]| Delta], Pairs), !,
	Indent1 is Indent + Delta,
	modifyindent(PrCRT, Pairs, [[No, Start, Indent1, LL| LT]| Acc], Result).
modifyindent([Line| PrCRT], Pairs, Acc, Result) :-
	modifyindent(PrCRT, Pairs, [Line| Acc], Result).



%%% margin(-Margin)
% Liefert diejenige Einrückungsweite M, von der anzunehmen ist, daß ein Lisp-
% Programmieranfänger eine gegebene Zeile im Normalfall nicht fälschlicherweise weiter
% als M bzw. weniger oder gleich weit einrückt.

margin(0).



%%% weaken(+Inconsistency, +Delta, -Inconsistency)

weaken([L1, L2, Type, Deg], Delta, [L1, L2, Type, Deg1]) :-
	Delta < Deg, !,
	Deg1 is Deg - Delta.
weaken([L1, L2, Type, Deg], Delta, [L1, L2, Type_c, Deg1]) :-
	Type_c is -Type,
	Deg1 is Delta - Deg.



%%% delta_test(+Line, +Delta, +Margin)
% Hilfsprädikat von modify_indent.
% Trifft zu, wenn die Verschiebung einer Indent Spalten weit eingerückten Zeile
% um Delta erlaubt ist.

delta_test([_, 0, Indent| _], Delta, M) :-
	Indent > M, !,
	Indent_ is Indent + Delta,
	Indent_ > M,
	Indent_ >= 0.

delta_test([_, 0, Indent| _], Delta, M) :-
	Indent =< M, !,
	Indent_ is Indent + Delta,
	Indent_ =< M,
	Indent_ >= 0.

% Falls das 1. Argument nur den letzten Abschnitt einer Zeile repräsentiert, gilt für
% Veränderungen der Einrückung nur die Beschränkung, daß die Einrückung positiv sein
% muß (nicht 0, weil Top-Level-Konstrukte jeweils auf einer neuen Zeile beginnen sollten).
delta_test([_, _, Indent| _], Delta, _) :-
	Indent_ is Indent + Delta,
	Indent_ > 0.



%%% delta_and_line(+Inconsistency, -Delta, ?Line)
% Liefert Werte für Delta (die Änderung der Einrückungsweite) und die Repräsentation
% der Zeile, deren Einrückung zu verändern ist. Die ersten beiden Klauseln sorgen
% dafür, daß, wenn die Wahl zwischen zwei Zeilenrepräsentationen besteht, von denen
% eine lediglich das Schlußsegment einer Zeile repräsentiert, nur diese als Line
% zurückgegeben wird.

delta_and_line(I, Delta, Line1) :-
	I = [Line1, Line2, _, _],
	Line1 = [_, Start| _],
	Line2 = [_, 0| _],
	Start > 0, !,
	d_and_l(I, Delta, Line1).
delta_and_line(I, Delta, Line2) :-
	I = [Line1, Line2, _, _],
	Line1 = [_, 0| _],
	Line2 = [_, Start| _],
	Start > 0, !,
	d_and_l(I, Delta, Line2).
delta_and_line(I, Delta, Line) :-
	I = [Line1, Line2, _, _],
	(Line = Line1;
	 Line = Line2),
	d_and_l(I, Delta, Line).


%%% d_and_l(+Inconsistency, -Delta, ?Line)
% Hilfsprädikat von delta_and_line.

% Gleiche ursprüngl. Einrückungsweite
d_and_l(I, Delta, Line) :-
	I = [Line1, Line2, Type, _],
	Line1 = [_, _, Indent| _],
	Line2 = [_, _, Indent| _], !,
	(increment_factor(Line1, IF),
	 Delta is -Type * IF,
	 Line = Line1;

	 increment_factor(Line2, IF),
	 Delta is Type * IF,
	 Line = Line2).

% Ungleiche ursprüngl. Einrückungsweite, Inkonsistenzgrad > 1
% In diesem Fall müßte sich das Vorzeichen der Einrückungsdifferenz umkehren, um
% die Inkonsistenz zu beheben. Wenn dies aus irgendeinem Grund nicht möglich ist,
% wird als Alternative angeboten, die beiden Zeilen gleich weit einzurücken.
d_and_l(I, Delta, Line) :-
	I = [Line1, Line2, Type, Degree],
	Line1 = [_, _, Indent1| _],
	Line2 = [_, _, Indent2| _],
	Degree > 1, !,
	(increment_factor(Line1, IF),
	 Delta is Indent2 - Indent1 - Type * IF,
	 Line = Line1;

	 increment_factor(Line2, IF),
	 Delta is Indent1 - Indent2 + Type * IF,
	 Line = Line2;

	 Delta is Indent2 - Indent1,
	 Line = Line1;

	 Delta is Indent1 - Indent2,
	 Line = Line2).

% Ungleiche ursprüngl. Einrückungsweite, Inkonsistenzgrad =< 1
d_and_l(I, Delta, Line) :-
	I = [Line1, Line2, _, _],
	Line1 = [_, _, Indent1| _],
	Line2 = [_, _, Indent2| _], !,
	(Delta is Indent2 - Indent1,
	 Line = Line1;

	 Delta is Indent1 - Indent2,
	 Line = Line2).


%%% increment_factor(+Line, -Factor)
% Hilfsprädikat von d_and_l. Sorgt dafür, daß Repräsentationen von Zeilen-Endsegmenten
% beliebig in ihrer 'Einrückung' verändert werden können, ohne mit 'normalen'
% Zeilenrepräsentationen zu interferieren. Dies wird einfach dadurch erreicht, daß
% die Einrückung einer Endsegment-Repräsentation niemals wieder eine ganze Zahl
% erreicht, wenn sie einmal verändert worden ist. Die einzige Ausnahme hiervon ist
% der Fall, daß die Layout-Konsistenz erfordert, daß die Einrückung einer
% Endsegmentrepräsentation gleich der einer normalen Zeilenrepräsentation ist.

increment_factor([_, 0| _], 1) :- !.
increment_factor([_, _, Indent| _], IF) :-
	num_to_list(Indent, 2, all, [Mag| List]),
	length(List, Ln),
	(Mag + 1 < Ln, !, % wenn Indent Stellen hinter dem Komma hat:
	 IF is 2 ** (Mag - Ln);
	 % sonst:
	 IF is 0.5).



%%% indentation_binder(+Delta1, +LineNo, +Direction, +PrCR, +PrCR_reversed, -Binder, -Delta2)
% Liefert der Reihe nach die Restlisten von PrCR, an deren jeweiliges erstes Element
% Zeile Nr. LineNo rekursiv Delta-einrückungsgebunden ist (mit Delta\=0).

indentation_binder(Delta1, No, down, PrCR, PrCRr, Binder, Delta2) :-
	first_member([No| LT], PrCR, PrCRT),
	indentationbinder(Delta1, down, [[No| LT]| PrCRT], PrCR, PrCRr, Binder, Delta2).
indentation_binder(Delta1, No, up, PrCR, PrCRr, Binder, Delta2) :-
	first_member([No| LT], PrCRr, PrCRTr),
	indentationbinder(Delta1, up, [[No| LT]| PrCRTr], PrCR, PrCRr, Binder, Delta2).


%%% indentationbinder(+Delta1, +Direction, +PrCRT, PrCR, +PrCR_reversed, -Binder, -Delta2)
% Hilfsprädikat zu indentation_binder.
% Nur dann, wenn Direction=up, sollte PrCRT die Restliste einer umgekehrten
% Prä-Konsistenzrepräsentation sein.

indentationbinder(Delta1, down, PrCRT, _, _, PrCRT, Delta1).
indentationbinder(Delta1, Dir, [[No, Start, Indent| LT]| PrCRT], PrCR, PrCRr, Binder,
		Delta3) :-
	successors(Indent, PrCRT, Successors),
	member([[No1, Start1, Indent1| LT1]| _], Successors),
	indentation_bound(Delta1, [No, Start, Indent| LT], [No1, Start1, Indent1| LT1],
		Delta2),
	(Indent = Indent1, !, Dir1 = Dir; Dir1 = _), % zur Vermeidung von Zyklen
	indentation_binder(Delta2, No1, Dir, PrCR, PrCRr, Binder, Delta3).


%%% indentation_bound(+Delta1, +Line1, +Line2, -Delta2)
% Gibt in Delta2 zurück, wieweit die Einrückung von Line2 verändert werden muß, wenn
% die von Line1 um Delta1 verändert wird, damit keine neue Inkonsistenz entsteht.
% Der Beweis schlägt fehl, wenn bereits eine Inkonsistenz zwischen Line1 und Line2
% besteht, oder wenn die Einrückung von Line2 gar nicht verändert zu werden braucht.

indentation_bound(Delta1, [No1, St1, Indent1| LT1], [No2, St_2, Indent2| LT2],
		Delta2) :-
	Delta1 \= 0,
	\+ inconsistent([No1, St1, Indent1| LT1], [No2, St_2, Indent2| LT2], _, _),
	DeltaI is Indent2 - Indent1,
	sign(DeltaI) =\= sign(Indent2 - Indent1 - Delta1),
	Delta2 is Delta1 - DeltaI + sign(DeltaI).




%%%% Behandlung von Klammerungsfehlern



%%% modify_ll(+PrCR1, +Suspects, +Type, +Degree, -PrCR3)
% Verändert, beginnend mit dem letzten Element von Suspects, die Layout-Ebene
% aller Elemente von PrCR1 um -Type * Degree.

modify_ll(PrCR1, [], _, _, PrCR1) :- !.
modify_ll(PrCR1, Suspects, Type, Degree, PrCR2) :-
	last([[Start| _]| _], Suspects),
	Delta is -Type * Degree,
	modifyll(PrCR1, Start, Delta, [], PrCR2).


%%% modifyll_(+PrCR1, +Start, +Delta, +Acc, -PrCR2)
% Hilfsprädikat von modify_ll.

modifyll([], _, _, Acc, PrCR) :- !,
	reverse(Acc, PrCR).
modifyll([Line| PrCR1], Start, Delta, Acc, PrCR2) :-
	Line = [No, _, _, _, StClP| _],
	(No < Start;
	 StClP = false,
	 No = Start), !,
	modifyll(PrCR1, Start, Delta, [Line| Acc], PrCR2).
modifyll([[No, St, Indent, LL| LT]| PrCR1], Start, Delta, Acc, PrCR2) :-
	LL1 is LL + Delta,
	modifyll(PrCR1, Start, Delta, [[No, St, Indent, LL1| LT]| Acc], PrCR2).



%%% get_end_column(+PrCR_tail, -End)
% Hilfsprädikat von repair_. Liefert die Endspaltennummer des Zeilenabschnitts, der in
% der Präkonsistenzrepräsentation unmittelbar vor PrCR_tail kommt, wenn der Abschnitt nicht
% bis zum Ende einer Zeile reicht, sonst wird 'end' zurückgegeben.

get_end_column([[_, Start| _]| _], End) :-
	Start > 0, !,
	End is Start - 1.
get_end_column(_, end).



%%% suspect_PrCRT(+PrCR_tail1, +PrCR_tail2, +Type, -Suspect_PrCR_tails)
% Liefert zu einer zwischen der ersten Zeile in PrCR_tail1 und der ersten Zeile in PrCR_tail2
% bestehenden Inkonsistenz vom Typ Type nacheinander alle PrCR_tails, deren erste Zeilen
% für eine diese Inkonsistenz abschwächende Modifikation in Frage kommen.
% Das Prädikat läßt sich am einfachsten als eine Funktion beschreiben, die eine
% Liste dieser "verdächtigen" Zeilen liefert:
/*
	Funktion suspect_PrCRTs(I)
	Input: eine Inkonsistenz I zwischen z und N(z, n) vom Typ t
	Liefert: eine Liste L der "verdächtigen" PrCR_tails

	L := suspects(t, z, N(z, n))

	Falls N(z, n) als erstes Nicht-Leerzeichen eine schließende Klammer enthält:
		L := append(L, [N(z, n)])
	__________

	Funktion suspects(t, z1, z2)
	Input: ein Inkonsistenztyp t, zwei Zeilennummern z1 u. z2, wobei z2 > z1
	Liefert: eine Liste L von "verdächtigen" PrCR_tails

	L := []
	Für alle N(z1, i) =< z2 in umgekehrter Reihenfolge:

		Falls N(z1, i) < z2:
			L := append(suspects(t, N(z1, i), N(z1, i+1)), L)
		Falls bound(t, z1, N(z1, i)):
			Abbruch der Schleife

	Falls die Schleife nicht abgebrochen wurde:
		L := append([z1], L)
	__________
*/


% Übergabe an das Hilfsprädikat suspect
suspect_PrCRT(PrCRT1, [[No2| _]| _], Type, PrCRT) :-
	suspect(Type, PrCRT1, No2, PrCRT).

% Die Endzeile der Inkonsistenz ist verdächtig, wenn sie mit einer schließenden Klammer
% beginnt.
suspect_PrCRT(_, PrCRT, _, PrCRT) :-
	PrCRT = [[_, _, _, _, true| _]| _].


%%% suspect(+Type, +PrCR_tail, +Line_no, -Line)
% Hilfsprädikat von suspect_PrCRT.
% Die erste Zeile im ersten Element von PrCR_tail ist die Anfangszeile der zu untersuchenden
% Inkonsistenz, Type ihr Typ, Line_no die Nummer ihrer Endzeile.
% Das Prädikat generiert zunächst die umgekehrte Liste der Nachfolger der Anfangszeile und
% läßt diese dann von suspect_ untersuchen.

suspect(Type, [[No1, St1, Indent1| LT1]| PrCRT1], End_no, Suspect) :-
	successors_(Indent1, PrCRT1, [], Successors),
	suspect_([[No1, St1, Indent1| LT1]| PrCRT1], Successors, Type, End_no, End_no, Suspect).


%%% suspect_(+PrCR_tail1, +PrCR_tails, +Type, +Line_no1, +Line_no2, -PrCR_tail2)
% Hilfsprädikat zu suspect.
% Die jeweils ersten Elemente von PrCR_tails sind in umgekehrter Reihenfolge die
% Nachfolger der ersten Zeile von PrCR_tail1. Diese werden nacheinander daraufhin
% untersucht, ob diese Zeile an sie gebunden ist. Line_no1 ist im ersten Aufruf
% die Nummer der Endzeile der Inkonsistenz; jenseits von ihr werden keine Zeilen betrachtet.
% Line_no2 ist die Nummer des jeweils zuletzt betrachteten Nachfolgers der ersten Zeile von
% PrCR_tail1 (im obigen Pseudocode N(z1, i+1)).

% Die erste Zeile in PrCRT1 ist an keine ihrer Nachfolgerinnen gebunden und damit verdächtig
suspect_(PrCRT1, [], _, _, _, PrCRT1) :- !.

% Ignorieren von Zeilen jenseits der Endzeile der Inkonsistenz
suspect_(PrCRT1, [[[No2| _]| _]| Tails], Type, _, End_no, Suspect) :-
	No2 > End_no, !,
	suspect_(PrCRT1, Tails, Type, No2, End_no, Suspect).

% Gegenwärtigen Nachfolger untersuchen, dann Rekursion zum vorhergehenden (dem nächsten
% in der Liste)
suspect_([Line1| PrCRT1], [[[No2| LT2]| _]| Tails], Type, _, End_no, Suspect) :-
	\+ bound(Type, Line1, [No2| LT2]),
	suspect_([Line1| PrCRT1], Tails, Type, No2, End_no, Suspect).

% Die Nachfolger der Nachfolger untersuchen
suspect_(_, [[[No2| LT2]| PrCRT2]| _], Type, No3, End_no, Suspect) :-
	No2 < End_no,
	suspect(Type, [[No2| LT2]| PrCRT2], No3, Suspect).



%%% filter_suspects(+Inconsistency, +PrCR, +SuspectPrCRTs1, -SuspectPrCRTs2)
% Gibt in SuspectPrCRTs2 dieselbe Liste wie SuspectPrCRTs1 zurück, schneidet
% diese aber, wenn der Typ von Inconsistency -1 ist, nach dem ersten Element,
% dessen Layout-Ebene < 0 ist, ab (wenn das erste nicht-Leerzeichen des Elements
% eine schl. Klammer ist; ansonsten unmittelbar vor diesem).
% Dieses Vorgehen ist insofern eine Vereinfachung, als bei einem multiplen
% Klammerungsfehler auch weitere Elemente von PrCR verdächtig sein können
% (wobei aber mindestens eine Korrektur schon vorher nötig ist). Um die Ausgabe
% aber nicht unnötig kompliziert zu machen, werden diese weggelassen.

filter_suspects([_, _, +1, _], _, Suspects, Suspects) :- !.
filter_suspects(_, PrCR, Suspects, Suspects1) :-
	filtersuspects(Suspects, PrCR, [], Suspects1).


%%% filtersuspects(+SuspectPrCRTs1, +PrCRT, +Acc, -SuspectPrCRTs2)
% Hilfsprädikat von filter_suspects.

filtersuspects([], _, Acc, Result) :- !,
	reverse(Acc, Result).
filtersuspects([Suspect| _], [Line| _], Acc, Result) :-
	Suspect = [[No, _, _, _, true| _]| _],
	Line = [No, _, _, LL| _],
	LL < 0, !,
	reverse([Suspect| Acc], Result).
filtersuspects(_, [Line| _], Acc, Result) :-
	Line = [_, _, _, LL| _],
	LL < 0, !,
	reverse(Acc, Result).
filtersuspects(SuspectPrCRTs, [Line| PrCRT], Acc, Result) :-
	fs_newAcc(SuspectPrCRTs, Line, Acc, SuspectPrCRTs1, Acc1),
	filtersuspects(SuspectPrCRTs1, PrCRT, Acc1, Result).


%%% fs_newAcc(+SuspectPrCRTs, +Line, +Acc1, -SuspectPrCRTs, -Acc2)
% Hilfsprädikat von filtersuspects.

fs_newAcc([[[No| LT]| PrCRT]| Rest], [No| _], Acc, Rest, [[[No| LT]| PrCRT]| Acc]) :- !.
fs_newAcc(SuspectPrCRTs, _, Acc, SuspectPrCRTs, Acc).



%%% bound(+Type, +Line1, +Line2)
% Trifft zu, wenn die Einrückungen und Layout-Ebenen von Line1 und Line2 sich so zueinander
% verhalten, daß eine Veränderung der Layout-Ebene von Line1 um -1 oder +1
% (je nach Type) zu einer Inkonsistenz führen würde.
% Das Prädikat setzt voraus, daß Line2 Nachfolger von Line1 ist.

bound(_, [_, _, Indent, LL| _], [_, _, Indent, LL| _]) :-
	!.
bound(-1, [_, _, Indent1, LL1| _], [_, _, Indent2, LL2| _]) :-
	Indent1>Indent2, !,
	LL2 is LL1 - 1.
bound(+1, [_, _, Indent1, LL1| _], [_, _, Indent2, LL2| _]) :-
	Indent1<Indent2,
	LL2 is LL1 + 1.





%%%%% Testprädikate:


/*

extri([], []).
extri([[[No1| _], [No2| _], Type, Degree]| R],
		[[No1, No2, Type, Degree]| R1]) :-
	extri(R, R1).

consis(Name, P, C) :-
	read_file(Name, [_, _| F]), pre_consistency(F, P), consistency(P, C).

chc(Name, OR) :-
	read_file(Name, RR), check_consistency(RR, OR).

stop.
*/