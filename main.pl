%%%%%%% SULLA (System zur Unterstütung bei der Lösung von Lisp-Aufgaben)






:- ensure_loaded('auxprd.pl').
:- ensure_loaded('parameters.pl').
:- ensure_loaded('read.pl').
:- ensure_loaded('analyse_misc.pl').
:- ensure_loaded('consistency.pl').
:- ensure_loaded('evaluate.pl').

:- dynamic user/5.





%%%%%% Die Benutzeroberfläche
%
% Repräsentation des ITS-Zustandes:
%
%	[Zustand der Benutzer-Oberfläche,
% 	 Leserepräsentation des Schülerprogramms,
%	 Leserepräsentationen der Musterlösungen,
%	 Schülermodell]
%
% Repräsentation des Schülerprogramms: s. read.pl
% Repräsentation der Musterlösungen:
%	Liste von Repräsentationen wie der des Schülerprogramms
% Schülermodell:
%	[Benutzername, Vorname, Benutzer-Dateipfad, Informationen über Resultate und
% 	 Hilfestellungen bei bisher bearbeiteten Aufgaben]
% Repräsentation von  Informationen über Resultate und Hilfestellungen bei bisher
%	bearbeiteten Aufgaben:
%	[[Augabenmarkierung, Informationen über gegebene Hilfestellungen und dafür
%	  berechneten Punktabzug, erreichte Punktzahl ohne Abzug für Hilfestellungen], ...]
% Informationen über gegebene Hilfestellungen und dafür berechneten Punktabzug:
% 	[[Beschreibung der Hilfestellung, Punktabzugs-Faktor], ...]
%
%
% Abkürzungen:
%
%  Zustand der Benutzer-Oberfläche: 			InterfaceState, IS
%  Leserepräsentation des Schülerprogramms:	ReadRepresentation, RR
%  Leserepräsentationen der Musterlösungen:	Exemplaries, Exm
%  Repräsentation des Schülermodells:			UserModel, UM
%  Ausgaberepräsentation:					OutputRepresentation, OR
%  Leseprozeß-Information:					ReadProcessInformation, RPI
%  Informationen über Resultate und
%    Hilfestellungen bei bisher bearbeiteten
%    Aufgaben (im Schülermodell):			UserHistory, UH
%  Informationen über gegebene Hilfestellungen
%    und dafür berechneten Punktabzug:  		HintInfo
%  Faktor, um den eine Punktzahl als Belohnung
%    oder Strafe verändert wird:			EvaluationFactor, EF
%  Beschreibung der Hilfestellung:			Hint
%  Benutzername:						Username, Unm
%  Vorname:							Forename, Fnm
%  Benutzer-Verzeichnispfad				Userpath, UP



%%%%% Initialisierung




%%% start

start :-
	welcome,
	(access_file('users.pl', exist), !,
	 load_files(['users.pl'], [silent(true)]);
	 true),
	load_exemplaries(Exm),
	log_in(Username, Forename, Userpath, UserHistory, Stop), !,
	(Stop = true)>+ % Aufhören, falls in der Login-Schleife ein EOF gelesen wurde
	personal_welcome(Forename),
	loop([start, _, Exm, [Username, Forename, Userpath, UserHistory]]).


%%% welcome

welcome :-
	nl,
	write('Willkommen bei SULLA.'), 
	nl, nl.


%%% personal_welcome(+Forname)

personal_welcome(Forename) :-
	nl, 
	writel(['Hallo, ', Forename, '!']),
	nl.


%%% log_in(-Username, -Forename, -Userpath, -UserHistory)

log_in(Username, Forename, Userpath, UserHistory, Stop) :-
	write('Benutzername: '), 
	line([Username| _], End1),
	Stop = false,
	write('Passwort: '), 
	line([Password| _], End2),
	((End1 = file; End2 = file), Stop = true)>+
	user(Username, Password, Forename, Userpath, UserHistory), !.
log_in(Username, Forename, Userpath, UserHistory, End) :-
	write('Benutzername/Passwort-Kombination unbekannt.'), nl, nl, !,
        sleep(1),
        log_in(Username, Forename, Userpath, UserHistory, End).



%%% load_exemplaries(-Exemplaries)

load_exemplaries(Exm) :-
	exemplaries_path(Path, "*.exm"),
	expand_file_name(Path, Listing),
	loadexm_(Listing, Exm).


%%% loadexm_(+FileNames, -Read_representations)
% Hilfsprädikat von load_exemplaries.

loadexm_([], []).
loadexm_([X| R], [File| R1]) :-
	read_file(X, File),
	loadexm_(R, R1).





%%%%%% Haupt-Interaktionsschleife




%%% loop(+State)

loop([end| _]) :- !.
loop(State) :-
	prompt(State),
	line(X, End),
	(End = file)>+
	eval(X, State, State1), !,
	loop(State1).





%%%%% Eingabeaufforderungen



%%% prompt(+State)

prompt([start| _]) :- !,
	nl, 
	write('Optionen: '), nl,
	write('1: Ein Lisp-Programm untersuchen lassen'), nl,
	write('2: Statistik ansehen'), nl,
	write('x: Sitzung beenden'), nl.
	
prompt([load_file| _]) :- !,
	nl, 
	write('Welche Programmdatei soll geladen werden? '), 
	nl.

prompt([[no_exercise, Tags], Program, Exm|  _]) :- !,
	exercise_tags(Program, Exm, Tags),
	(Tags=[], !,
	 write('Keine gültigen Aufgabenmarkierungen gefunden!'), nl, nl,
	 write('Optionen:'), nl,
	 write('a : andere Datei; e : erneut laden; x : zurück zum Anfang'), nl;

	 write('Welche Aufgabe soll untersucht werden?'), nl,
	 write('Die folgenden Nummern stehen zur Verfügung:'), nl,
	 write_items(Tags), nl, nl,
	 write('(a : andere Datei laden; x : zurück zum Anfang)'), nl).

prompt([error_in_file| _]) :- !,
	write('Optionen:'), nl,
	write('a : andere Datei; e : erneut laden; x : zurück zum Anfang'), nl.

prompt([[exercise, _, false]| _]) :- !,
	write('Optionen:'), nl,
	write('e : erneut laden; h : Hinweis; a : andere Aufgabe; x : zurück zum Anfang'), nl.

prompt([[exercise, _, true]| _]) :-  !,
	write('Optionen:'), nl,
	write('e : erneut laden; a : andere Aufgabe; x : zurück zum Anfang'), nl.





%%%%% Befehlsausführung



%%% eval(+Input, +State1, -State2)

eval([1], [start| R], [load_file| R]) :- !.

eval([2], [start| R], [start| R]) :- !,
	show_results([start| R]).	

eval([x], [start| R], State1) :- !,
	bye([start| R], State1).

eval(FileName, State, State1) :-
	State = [load_file, _, _, [_, _, Userpath, _]], 
	FileName \= [''], !,
	concat_atom(FileName, ' ', FileName1),
	concat(Userpath, FileName1, FileName2),
	load_file(FileName2, State, State1).

eval([Tag], State, State1) :-
	State = [[no_exercise, Tags]| _], 
	member(Tag, Tags), !,
	see_exercise(Tag, State, State1).

eval([h], State, State1) :-
	State = [[exercise| _]| _], !,
	give_hint(State, State1).

eval([e], State, State1) :- !,
	load_file(_, State, State1).

eval([a], [[exercise| _]| R], [[no_exercise, _]| R]) :- !.

eval([a], [_| R], [load_file| R]) :- !.

eval([x], [_| R], [start| R]) :- !.


eval(_, S, S) :- !.


%% Zum Debuggen die letzte Klausel auskommentieren. Die folgende Klausel gibt nähere
% Informationen:

eval(I, [S| R], [S| R]) :-
	write('Could not interpret input.'), nl,
	write('Input-stack is as follows:'), nl,
	write(I), nl,
	write('State of interface is: '), write(S), nl.




%%%%% Hilfsprädikate von eval




%%% bye(+State1, -State2)

bye([_| R], [end| R]) :-
	write('Bis bald!'), nl.




%%%% Lisp-Programme laden



%%% load_file(+FileName, +State1, -State2)
% Lädt ein Schülerprogramm.

load_file(FileName, State, State1) :-
	(State = [_, [FileName| _]| _]; true), !,
	writel(['Lade Datei ', FileName, '...']),
	read_file(FileName, RR),
	RR = [_, RPI| RRT],
 	(RRT=[], !, OR = [];
 	 check_consistency(RR, OR)),
 	delete(RPI, error(_, _, end(l)), RPI1), % falls die Datei innerhalb einer Liste endet, 
 				% hat sich check_consistency schon darum gekümmert.
 	file_diagnosis(OR, RPI1, CorrectionNeeded, Diagnosis, []),
	nl, nl, writel(Diagnosis),
	new_state_after_load(FileName, RR, CorrectionNeeded, State, State1).


%%% file_diagnosis(+Output_Representation, +Read_Process_Information,
%%%	-Correction_needed, -Diagnosis, ?Rest)
% Hilfsprädikat von load_file.

file_diagnosis([], [], false) --> !.
file_diagnosis(OR, [], true) --> !,
	or_text(OR), nl.
file_diagnosis([], RPI, CN) --> !,
	rpi_text(RPI, CN), nl.
file_diagnosis(OR, RPI, true) -->
	or_text(OR), nl, ['Des weiteren: '], nl,
	rpi_text(RPI, _), nl.


%%% new_state_after_load(+FileName, +Read_representation, +Correction_needed, +State1,
%%%	-State2)
% Hilfsprädikat von load_file.
% Liefert einen neuen Zustand des Systems, nachdem ein Schülerprogramm geladen worden ist.
% Wenn im ursprünglichen Zustand eine Aufgabe bearbeitet worden ist, wird diese erneut
% untersucht.

% Fehler im Programm
new_state_after_load(FileName, _, true, [_, _| R], [error_in_file, [FileName]| R]) :- !.

% Aufgabe erneut untersuchen
new_state_after_load(_, RR, false, [[exercise, Tag, _], _, Exm, UM], State1) :-
	RR = [_, _| RRT],
	member([Tag| _], RRT), !,
	see_exercise(Tag, [_, RR, Exm, UM], State1).

new_state_after_load(_, RR, false, [_, _| R], [[no_exercise, _], RR| R]).




%%%% Aufgaben beurteilen



%%% see_exercise(+State1, -State2)

see_exercise(Tag, State, [[exercise, Tag, Defect]| State1_tail]) :-
	State = [_, RR, Exm, [Username, Forename, Userpath, UH]],
	eval_exercise(Tag, RR, Exm, [Username, Forename, Userpath, UH],
		OR, Reachable, Result),
	exercise_diagnosis(OR, Defect, Diagnosis, []),
	evaluation(Tag, Reachable, Result, Defect, UH, UH1, Evaluation, []),
	nl, writel(Diagnosis),
	nl, writel(Evaluation), nl, nl,
	replace_UH(UH1, State, [_| State1_tail]).


%%% evaluation(+Tag, +Reachable, +Result, +Defect, +UH1, -UH2, -Evaluation, ?Rest)
% Hilfsprädikat von see_exercise. Liefert zu einer Aufgabe den abschließenden Bewertungstext
% (Evaluation) sowie eine modifizierte Benutzergeschichte.

evaluation(_, _, _, true, UH, UH) --> !,
	['Keine Bewertung der Aufgabe möglich.'].
evaluation(Tag, Reachable, Result, false, UH, [[Tag, Reachable, HintInfo, Result]| UH1]) -->
	{delete(UH, [Tag, _, HintInfo, _], UH1),
	 (nonvar(HintInfo), !;
	  HintInfo=[]),
	 get_result(HintInfo, Result, Result1)},
	['(Vorläufiges) Ergebnis für diese Aufgabe: ', Result1, ' von ',
         Reachable, ' Punkten.'].




%%%% Statistik




%%% show_results(+State)
% Zeigt dem Benutzer die bisher erreichten Ergebnisse für die bearbeiteten Aufgaben,
% als auch, welche Aufgaben noch bearbeitet werden können.

show_results(State) :-
	results_text(State, Text, []),
	writel(Text).
	

%%% results_text(+State, -Text, ?Rest)
% Liefert den Text für show_results.

results_text(State) -->
	resultstext(State, Reachable0, Reachable1, Reached),
	not_attempted_text(State, Reachable2),
	summary_text(Reached, Reachable0, Reachable1, Reachable2),
	nl.


%%% resultstext(+State, -Text, ?Rest)
% Hilfsprädikat von results_text. Gibt ggf. eine Tabelle zurück, die Informationen über
% bisher erreichte Punktzahlen und Abzüge enthält.

resultstext([_, _, _, [_, _, _, []]], 0, 0, 0) --> !,
	nl, ['Sie haben noch keine Aufgaben bearbeitet.'], nl.
resultstext([_, _, _, [_, _, _, UH]], Reachable0, Reachable1, Reached) -->
	{goalsort(UH, compare, E, X, E=[X| _], UH_s)},
	nl,
	['Aufgabe \tErreichbare \tErreichte \tMit Punktabzug'], nl,
	['        \tPunktzahl   \tPunktzahl \tfür Hilfestellungen'], nl,
	['--------\t------------\t----------\t--------------------'], nl,
	resultstext_(UH_s, 0, 0, 0, Reachable0, Reachable1, Reached), 
	nl, nl.


%%% resultstext_(+UH, +Reachable0_Acc, +Reachable1_Acc, +Reached_Acc,
%%%	-Reachable0, -Reachable1, -Reached, -Text, ?Rest)
% Hilfsprädikat von showresults. Gibt die einzelnen Zeilen der Ergebnistabelle zurück.

resultstext_([], R0a, R1a, Ra, R0a, R1a, Ra) --> !,
	[].
resultstext_([[Tag, Reachable, HintInfo, Result1]| Rest], R0a, R1a, Reached_a, R0, R1, Reached) -->
	{get_result(HintInfo, Result1, Result2),
	 Result1r is round(Result1, -1),
	 Result2r is round(Result2, -1),
	 Percent1 is (Result1r / Reachable) * 100,
	 Percent2 is (Result2r / Reachable) * 100,
	 R0a1 is R0a + Reachable,
	 get_result(HintInfo, 1, HintEF),
	 R1a1 is R1a + HintEF * Reachable,
	 Reached_a1 is Reached_a + Result2},
	[Tag, ' \t\t',  Reachable, ' \t\t', Result1r, ' (', Percent1, '%)   \t',
		Result2r, ' (', Percent2, '%)'],
	nl,
	resultstext_(Rest, R0a1, R1a1, Reached_a1, R0, R1, Reached).
	

%%% not_attempted_text(+State, -Reachable, -Text, ?Rest)
% Hilfsprädikat von show_results. Liefert Informationen über noch nicht 
% bearbeitete Aufgaben und gibt auch die Summe der dort zu erreichenden Punkte zurück.

not_attempted_text([_, _, Exm, _], 0) -->
	{\+ (find_tag(Tag, Exm, _, _), Tag \= 0), !},
	['Es liegen keine Aufgaben/Musterlösungen vor.'].
not_attempted_text([_, _, Exm, [_, _, _, UH]], Reachable) -->
	{findall([Tag| Directives], (
			find_tag(Tag, Exm, [_, Directives| _], _),
			Tag \= 0,
			\+ member([Tag| _], UH)
		), Tag_Dirs_pairs)},
	({Tag_Dirs_pairs = [], !,
	 Reachable = 0},
	 ['Sie haben bereits alle Aufgaben bearbeitet.'];

	 {goalsort(Tag_Dirs_pairs, compare, E, X, E = [X| _], TDPairs),
	  points_marker(PM)},
	 ['Noch nicht bearbeitete Aufgaben: '],
	 notattempted(TDPairs, PM, 0, Reachable)).


%%% notattempted(+Tag_directives_pairs, +Points_marker, +Acc, -Reachable)
% Hilfsprädikat von not_attempted.

notattempted([], _, R, R) --> !,
	[].
notattempted([[Tag| Dirs]| TDPairs], PM, Acc, Reachable) -->
	{(member(d([PM, Points| _]), Dirs),
	  number(Acc), !,
	  Acc1 is Acc + Points;

	  Points = ?,
	  Acc1 = ?)},

	[Tag, ' (', Points, ' P.)'],
	separate(', ', TDPairs),
	notattempted(TDPairs, PM, Acc1, Reachable).


%%% summary_text(+Reached, +Reachable_on_attempted_exercises,
%%% 	+Reachable_on_attempted_exercises_with_deduction_for_hints,
%%%	+Reachable_on_not_attempted_exercises, -Text, ?Rest)
% Hilfsprädikat von results_text.

summary_text(_, _, _, ?) --> !,
	[].
summary_text(_, 0, _, 0) --> !,
	[].
summary_text(Reached, Reachable0, Reachable1, Reachable2) -->
	{Reached_r is round(Reached, -1),
	 Reachable_absolute is Reachable0 + Reachable2,
	 Reachable_with_hints is Reachable1 + Reachable2,
	 Percent1 is round((Reached_r / Reachable_absolute) * 100, -1),
	 Percent2 is round((Reachable_with_hints / Reachable_absolute) * 100, -1)},
	nl, ['Insgesamt erreicht: ', Reached_r, ' von ', Reachable_absolute, 
		' Punkten oder ', Percent1, '%'], 
	({Reached < Reachable_absolute, !},
	 nl, ['Abzüglich der bereits gegebenen Hilfestellungen noch erreichbar: ',
	 	Percent2, '%'];
	 []),
	nl.





%%%% Lösungshinweise



%%% give_hint(+State1, -State2)
% Erstellt zuerst (mit hint_menu, nachdem die notwendigen Daten beisammen sind) 
% ein Menü über alle verfügbaren Lösungshinweise, läßt den Benutzer dann einen 
% solchen auswählen, zeigt schließlich den entsprechenden Text an und 
% aktualisiert die Benutzergeschichte.

give_hint(State, State1) :-
	State = [_, _, _, [_, _, _, UH]],
	findall(ID, clause(get_hint(ID, _, _, _, _, _, _, _), _), IDs),
	delete(UH, [Tag, Reachable, HintInfo, Result], UH1),
	hint_menu(IDs, State, 0, HintInfo, Text_HintInfo_pairs, MaxItemNo, Menu, []),
	write('Folgende Lösungshinweise sind verfügbar: '), nl,
	writel(Menu),
	MaxItemNo1 is MaxItemNo + 1,
	user_input(['Geben Sie bitte eine ganze Zahl zwischen 0 und ', MaxItemNo1, ' an: '],
		L, (L=[N| _], integer(N), between(1, MaxItemNo, N))),
	nl,
	nth1(N, Text_HintInfo_pairs, [Text| HintInfo1]),
	writel(Text), nl,
	replace_UH([[Tag, Reachable, HintInfo1, Result]| UH1], State, State1).



%%% hint_menu_(+HintIDs, +State, +ItemNo, +ExmLines, +HintInfo, 
%%%	-Text_HintInfo_pairs, -MaxItemNo, -Menu, ?Rest)
% Erstellt mit Hilfe von get_hint ein Menü für Lösungshinweise, und gibt außerdem in
% Text_UH_pairs Paare von Texten und Benutzergeschichten zurück. Für diese gilt:
% Ist [Text_i| UH_i] das i-te Paar und wird das i-te Element des Menüs ausgewählt, ist
% der Hinweistext Text_i auszugeben und die Benutzergeschichte durch UH_i zu ersetzen.
% Schließlich wird in MaxItemNo noch die größte erreichte Elementzahl ausgegeben. Diese 
% ist nicht zwangsläufig gleich der Länge von HintIDs, da erst get_hint darüber 
% entscheidet, ob ein bestimmter Hinweistyp tatsächlich verfügbar ist.

hint_menu([], _, ItemNo, _, [], ItemNo) --> !,
	[].
hint_menu([HID| HintIDs], State, ItemNo, HintInfo, 
		[[Text| HintInfo1]| Text_HintInfo_pairs], MaxItemNo) -->
	{ItemNo1 is ItemNo + 1},
	[ItemNo1, ': '],
	get_hint(HID, State, HintInfo, Percent, Text, HintInfo1), !,
	nl, ['    (Punktabzug: ', Percent, '%)'], nl,
	hint_menu(HintIDs, State, ItemNo1, HintInfo, Text_HintInfo_pairs, MaxItemNo).
hint_menu([_| HintIDs], State, ItemNo, HintInfo, Text_HintInfo_pairs, MaxItemNo) -->
	hint_menu(HintIDs, State, ItemNo, HintInfo, Text_HintInfo_pairs, MaxItemNo).



%%% get_hint(+HintID, +State, +ExmLines, +HintInfo1, 
%%%	-Percent, -Text, -HintInfo2, -MenuItem, ?Rest)
% Hilfsprädikat von hint_menu.
% Liefert alle benötigten Informationen zu einem Lösungshinweis vom Typ HintID.
% Beweis schlägt fehl, wenn der Hinweistyp nicht verfügbar ist.

get_hint(levels_new, State, HintInfo, Percent, Text,
		[[levels(Level1), EF1]| HintInfo1], MenuItem, Rest) :-
	delete(HintInfo, [levels(Level), EF], HintInfo1),
	(nonvar(Level), !;
	 Level = -1, EF = 1),
	EF \= 0, % (sonst wären schon alle Ebenen aufgedeckt worden)
	Level1 is Level + 1,
	levels_extract(State, ExmLines),
	levels_EF(Level1, ExmLines, EF1),
	Percent is (1 - EF1 / EF) * 100,
	levels_text(Level1, ExmLines, Text, []),
	MenuItem = ['Aufdecken der Musterlösung bis zur ', Level1, 'ten Ebene'| Rest].

get_hint(levels_old, State, HintInfo, 0, Text, HintInfo, MenuItem, Rest) :-
	member([levels(Level), _], HintInfo),
	levels_extract(State, ExmLines),
	levels_text(Level, ExmLines, Text, []),
	MenuItem = ['Erneutes Aufdecken der Musterlösung bis zur ', Level, 'ten Ebene'| Rest].

get_hint(operators_new, State, HintInfo, Percent, Text, 
		[[operators, EF]| HintInfo], MenuItem, Rest) :-
	\+ member([operators, _], HintInfo),
	operators_hint_eval_factor(EF),
	Percent is (1 - EF) * 100,
	operators_extract(State, Operators),
	operators_text(Operators, Text, []),
	MenuItem = ['Auflisten von in der Musterlösung verwendeten Operatoren'| Rest].

get_hint(operators_old, State, HintInfo, 0, Text, HintInfo, MenuItem, Rest) :-
	member([operators, _], HintInfo),
	operators_extract(State, Operators),
	operators_text(Operators, Text, []),
	MenuItem = ['Erneutes Auflisten von in der Musterlösung verwendeten Operatoren'| Rest].


%%% levels_extract(+State, -ExmLines)
% Hilfsprädikat von get_hint. Dient dem Extrahieren von Informationen aus dem globalen
% Zustand für Hinweise vom Typ levels_old oder levels_new.

levels_extract([[exercise, Tag, false], _, Exm| _], Lines1) :-
	find_tag(Tag, Exm, [_, _, Lines], _), !,
	trim_line_list(Lines, Lines1).


%%% levels_EF(+Level, +Lines, -EF)
% Hilfsprädikat von get_hint. Liefert zu einer Aufdeckungsebene Level sowie den einer
% Musterlösung entsprechenden Zeilenrepräsentationen Lines den entsprechenden
% Evaluierungsfaktor.

levels_EF(Level1, Lines, EF) :-
	findall(Objects,
		(get_objects(Lines, Level1, symbol_or_string, Objects, Limit, RB),
		 (Limit = Level1; RB = true)),
		[Objects1| ObjectsR]),
	(ObjectsR = [], EF = 0)>+
	ObjectsR = [AllObjects],
	length(AllObjects, AON),
	length(Objects1, O1N),
	EF is 1 - O1N / AON.


%%% symbol_or_string(+Line_tail, -_)
% Hilfsprädikat von levels_EF, zum Übergeben an get_objects.

symbol_or_string([[sym| _]| _], _) :- !.
symbol_or_string([[symbol| _]| _], _) :- !.
symbol_or_string([[string| _]| _], _).


%%% levels_text(+Level, +Lines, -Text, ?Rest)
% Hilfsprädikat von get_hint.

levels_text(Level, Lines) -->
	{reconstruct(Level, Lines, Lines1)},
	['Die Musterlösung bis zur ', Level, 'ten Ebene lautet:'],
	nl, nl, Lines1.



%%% operators_extract(+State, -Operators)
% Hilfsprädikat von get_hint. Dient dem Extrahieren von Informationen aus dem globalen 
% Zustand für Hinweise vom Typ operators_old oder operators_new.

operators_extract([_, _, _, [Username| _]], Operators) :-
	opsfile_name(Username, OpsFN),
	see(OpsFN),
	line(Operators),
	seen.


%%% operators_text(+Operators, -Text, ?Rest)
% Hilfsprädikat von get_hint.

operators_text(Operators) -->
	{newline_char(Nl)},
	['In der Musterlösung verwendete Operatoren: '],
	attach(Operators, [Nl]).





%%%%% Interpretation von Ausgaberepräsentationen





%%%% Ausgabe von check_consistency





%%% or_text(+OR, -Diagnosis, ?Rest)
% Liefert eine (natürlichsprachliche) Interpretation einer von check_consistency erzeugten
% Ausgaberepräsentation.

or_text([]) --> !.
or_text(OR) -->
	['Layout und Klammerung sind nicht konsistent.'], nl,
	or_text_(OR).


or_text_([]) --> !.
or_text_([Element| ORT]) -->
	{Element = [_, _, parentheses| _], !},
	p_text(Element), nl,
	or_text_(ORT).
or_text_([Element| ORT]) -->
	{Element = [_, _, indentation| _], !},
	i_text(Element), nl,
	or_text_(ORT).



%%% p_text(+OR_element, -Text, ?Rest)
% Liefert zu einem Element der von check_consistency erzeugten Ausgaberepräsentation die
% entsprechende Vermutung über die Orte überzähliger oder fehlender Klammern.

p_text([No1, No2, parentheses, Start, Type, Degree, Suspects]) -->
	p_header(No1, No2), nl,
	p_description(Type, Degree, No1, Start, Suspects).

p_header(No, No) --> !,
	['Z. ', No, ': '],
	p_title.
p_header(No1, No2) -->
	['Z. ', No1, ' - ', No2, ': '],
	p_title.

p_title -->
	['Layout irreleitend und/oder Klammerungsfehler:'].

p_description(Type, Degree, No1, Start, Suspects) -->
	p_type(Type),
	[' (', Degree, 'mal) in: '],
	p_suspects(No1, Start, Suspects).


p_type(-1) --> !,
	['Vermutlich zu viele \')\' oder zu wenige \'(\''].
p_type(+1) -->
	['Vermutlich zu viele \'(\' oder zu wenige \')\''].


p_suspects(No1, Start, [S]) --> !,
	p_suspect(No1, Start, S).
p_suspects(No1, Start, [S1, S2]) --> !,
	p_suspect(No1, Start, S1), [' oder '], p_suspect(No1, Start, S2).
p_suspects(No1, Start, [S| R]) -->
	p_suspect(No1, Start, S), [', '],
	p_suspects(No1, Start, R).


% Wenn die verdächtige Zeile die erste in der Inkonsistenz ist, die hierfür vorgesehene
% Startspaltennummer anstelle der Endspaltennummer benutzen (sofern größer als 0)
p_suspect(No1, Start, [No1, _]) -->
	{Start > 0, !},
	['Z. ', No1, ' ab Sp. ', Start].

p_suspect(_, _, [No, end]) -->  !,
	['Z. ', No].

p_suspect(_, _, [No, End]) -->
	['Z. ', No, ' bis einschl. Sp. ', End].



%%% i_text(+OR_element, -Text, ?Rest)
% Liefert zu einem Element der von check_consistency erzeugten Ausgaberepräsentation die
% entsprechende Anweisung, die Einrückung einer Zeile zu verändern.

i_text([No, No, indentation, Delta]) -->
	{Delta < 0, !, Delta1 is abs(Delta)},
	['Z. ', No, ' sollte ', Delta1, ' '],
	n(column, Delta1),
	[' weniger weit eingerückt sein.'].

i_text([No, No, indentation, Delta]) -->
	{Delta > 0, !},
	['Z. ', No, ' sollte ', Delta, ' '],
	n(column, Delta),
	[' weiter eingerückt sein.'].

i_text(_) -->
	[].


%%% n(+Sem, +Num, -Text, ?Rest)
% Ein wenig "Grammatik"...

n(parenthesis, 1) --> !,
	['Klammer'].
n(parenthesis, _) -->
	['Klammern'].
n(column, 1) --> !,
	['Spalte'].
n(column, _) -->
	['Spalten'].



%%% rpi_text(+RPI, -Correction_needed, -Diagnosis, ?Rest)
% Liefert eine (natürlichsprachliche) Interpretation einer von read_file oder read_object
% erzeugten Leseprozeß-Informationen.

rpi_text(RPI, true) -->
	{member(file_error(does_not_exist, Name), RPI), !},
	['Die Datei ', Name, ' existiert nicht.'], nl.
rpi_text(RPI, true) -->
	{member(file_error(no_read_access, Name), RPI), !},
	['Die Datei ', Name, ' kann nicht gelesen werden.'], nl.
rpi_text(RPI, CN1) -->
	{extract_list(tabs/2, RPI, [], [], Tabs, RPI_rest)},
	rpi_tabs(Tabs, false, CN0), nl,
	{extract_list(error/3, RPI_rest, [], [], Errors, _)},
	rpi_errors(Errors, CN0, CN1).


rpi_tabs([], CN, CN) --> !.
rpi_tabs([tabs(L, N)| _], CN, CN) -->
	['Der Code enthält Tabs (ab Z. ', L, '). Für die Layoutanalyse'], nl,
	['wurden Tabs übersetzt in jeweils ', N, ' Leerzeichen.'].


rpi_errors([], CN, CN) --> !.
rpi_errors([error(Line, Char, ID)| Errors], _, true) -->
	['Zeile: ', Line, ', Zeichen: ', Char, ', Fehler:'], nl,
	rpi_error(ID), nl,
	rpi_errors(Errors, _, _).


rpi_error(closing_parenthesis) -->
	['Ein Objekt kann nicht mit einer schließenden Klammer beginnen.'].

rpi_error(colon) -->
	['Die Repräsentation eines Symbols kann höchstens an einer Stelle'], nl,
	['ungeschützte Doppelpunkte enthalten, und zwar höchstens zwei.'].

rpi_error(comma(Sign)) -->
	['Ein "', Sign, '" ist nur in Listen erlaubt, allerdings auch'], nl,
	['da nicht im cdr-Teil derselben (d.h. nach dem "." in einer dotted list).'].

rpi_error(dot) -->
	['"." ist nur im unmittelbaren Kontext einer Liste sinnvoll einlesbar,'], nl,
	['"..", "..." usw. gar nicht.'].

rpi_error(no_car) -->
	['Fehlerhafte Liste: ".", aber kein erstes Element.'].

rpi_error(dotted_list) -->
	['Der cdr-Teil einer dotted list muß genau ein Element enthalten.'].

rpi_error(escape_char) -->
	 ['Die Datei endet mit einem ungeschützten Escape-Zeichen (', '''', \, '''', ').'].

rpi_error(hash_sign) -->
	['Freistehende "#" bzw. "#n" (wobei n eine Ziffernfolge ist) und'], nl,
  	['mit "#<" beginnende Ausdrücke sind nicht erlaubt.'].

rpi_error(no_backquote) -->
	['Einem Komma konnte kein entsprechendes Backquote zugeordnet werden.'].

rpi_error(end([string| _])) -->
	['Die Datei endet innerhalb eines Strings.'].

rpi_error(end(_)) -->
	['Die Datei endet innerhalb eines Objekts.'].



%%% exercise_diagnosis(+OR, -Defect, -Diagnosis, ?Rest)
% Liefert eine (natürlichsprachliche) Interpretation einer von eval_exercise erzeugten
% Ausgaberepräsentation. In Defect wird zurückgegeben, ob die Aufgabe aus irgendeinem
% Grund nicht bewertet werden kann.

exercise_diagnosis(OR, true) -->
	{var(OR), !},
	[].
exercise_diagnosis(OR, true) -->
	{delete1(OR, lisp_eval, OR1), !},
	['Fehler beim Aufruf des Lisp-Interpreters.'],
	exercise_diagnosis(OR1, _).
exercise_diagnosis(OR, true) -->
	{delete1(OR, file_error(read, FN), OR1), !},
	['Die Datei ', FN, ' kann nicht gelesen werden.'],
	exercise_diagnosis(OR1, _).
exercise_diagnosis(OR, true) -->
	{delete1(OR, file_error(exist, FN), OR1), !},
	['Die Datei ', FN, ' existiert nicht.'],
	exercise_diagnosis(OR1, _).
exercise_diagnosis(OR, true) -->
	{delete1(OR, file_error(access, FN), OR1), !},
	['Auf die Datei ', FN, ' kann nicht zugegriffen werden.'],
	exercise_diagnosis(OR1, _).
exercise_diagnosis(OR, Defect) -->
	{extract_list(exm_error/2, OR, [], [], ExmErrors, OR_rest1)},
	exm_text(ExmErrors, false, Defect1),
	{extract_list(error/2, OR_rest1, [], [],  Errors, OR_rest2)},
	error_text(Errors, Defect1, Defect2),
	{extract_list(trace/3, OR_rest2, [], [], Trace, _)},
	trace_text(Trace, Defect2, Defect).


%%% exm_text(+OR, +Defect1, -Defect2, -Text, ?Rest)
% Hilfsprädikat von exercise_diagnosis.
% Interpretiert den Teil der von eval_exercise erzeugten Ausgaberepräsentation, der
% von Fehlern in der Musterlösungsdatei handelt. Liegt ein solcher Fehler vor,
% kann die Aufgabe nicht bewertet werden, und dementsprechend wird in Defect2 true
% zurückgegeben (sonst Defect1).

exm_text([], Defect, Defect) --> !.
exm_text([Error| R], Defect, Defect2) -->
	exm_error(Error, Defect, Defect1), nl,
	exm_text(R, Defect1, Defect2).

exm_error(exm_error(directives, Report), _, true) -->
	['Eine Direktive enthält eine fehlerhafte Repräsentation eines'],
	nl, ['Lisp-Objekts. Fehlermeldung: '],
	nl, attach_lines(Report).
exm_error(exm_error(loading, Report), _, true) -->
	['Fehler beim Laden der Musterlösungsdatei:'],
	nl, attach_lines(Report).
exm_error(exm_error(parm_lists, Report), _, true) -->
	['Fehler beim Erzeugen der Parameterlisten:'],
	nl, attach_lines(Report).
exm_error(exm_error(equivalence, Report), _, true) -->
	['Fehler beim Zugriff auf die Äquivalenzfunktion:'],
	nl, attach_lines(Report).
exm_error(exm_error(function_call, [Parms, Report]), _, true) -->
	['Fehler beim Aufruf der Musterfunktion mit'],
	nl, ['folgender Parameterliste:'],
	nl, Parms,
	nl, ['Fehlermeldung:'],
	nl, attach_lines(Report).
exm_error(exm_error(comparing, [Result1, Result2, Report]), _, true) -->
	['Fehler beim Vergleich der Resultatwertlisten.'],
	nl, ['Resultatwertliste der Musterlösungsfunktion:'],
	nl, Result1,
	nl, ['Resultatwertliste Ihrer Funktion:'],
	nl, Result2,
	nl, ['Fehlermeldung:'],
	nl, attach_lines(Report).
exm_error(exm_error(missing_dir, points), _, true) -->
	['Musterlösungsdatei:'],
	nl, ['Fehlende Angabe zur zu erreichenden Punktzahl.'].
exm_error(exm_error(missing_dir, equivalence), _, true) -->
	['Musterlösungsdatei:'],
	nl, ['Fehlende Angabe zum zu verwendenden Äquivalenztest.'].
exm_error(exm_error(missing_dir, function), _, true) -->
	['Musterlösungsdatei:'],
	nl, ['Fehlende Angabe zur zu testenden Funktion (bzw. zum zu testenden Makro).'].
exm_error(exm_error(missing_dir, test_parameters), _, true) -->
	['Musterlösungsdatei:'],
	nl, ['Fehlende Angaben zu den zu verwendenden Parameterlisten.'].
exm_error(exm_error(corrupt_file, FileName), _, true) -->
	['Fehlerhaftes Protokoll:', FileName].


%%% error_text(+OR, +Defect1, -Defect2, -Text, ?Rest)
% Hilfsprädikat von exercise_diagnosis.
% Interpretiert den Teil der von eval_exercise erzeugten Ausgaberepräsentation, der
% von Fehlern im Benutzerprogramm handelt. Funktioniert ansonsten wie exm_text.

error_text([], Defect, Defect) --> !.
error_text([Error| R], Defect, Defect2) -->
	error(Error, Defect, Defect1), nl,
	error_text(R, Defect1, Defect2).

error(error(loading, Report), Defect, Defect) -->
	['Fehler beim Laden Ihres Programms:'],
	nl, attach_lines(Report).
error(error(function_call, [Parms, Report]), Defect, Defect) -->
	['Fehler beim Aufruf Ihrer Funktion (oder Ihres Makros) mit '],
        nl, ['folgender Parameterliste:'],
	nl, Parms,
	nl, ['Fehlermeldung:'],
	nl, attach_lines(Report).
error(error(corrupt_file, FileName), _, true) -->
	['Fehlerhaftes Protokoll: ', FileName].


%%% trace_text(+OR, +Defect1, -Defect2, -Text, ?Rest)
% Hilfsprädikat von exercise_diagnosis.
% Interpretiert den Teil der von eval_exercise erzeugten Ausgaberepräsentation, der
% von Unterschieden in der Effizienz von Muster- und Benutzerlösung handelt. In
% Defect2 wird true zurückgegeben, wenn die von eval_exercise ausgewertete 
% Protokolldatei fehlerhaft ist, sonst Defect1.


trace_text([], Defect, Defect) --> [].
trace_text(Trace, _, true) -->
	{member(trace(file_corrupt, FileName, _), Trace)}, !,
	['Fehlerhaftes Protokoll: ', FileName].
trace_text(Trace, Defect, Defect1) -->
	{member(trace(equivalence, CEs, CEF), Trace), CEs=[_| _]}, !,
	['Ihre Lösung ist noch nicht korrekt. Für folgende Parameterlisten '],
	nl, ['liefert sie falsche Resultate: '],
	{shown_counter_examples(CEN)},
	nl, attach_first(CEs, [CEN, all], ['', ''], '...'),
	{Percent is (1 - CEF) * 100},
	nl, ['Damit würden Sie ', Percent, ' Prozent Ihrer Punktzahl für diese Aufgabe'],
	nl, ['verlieren.'],
	({Percent=100, !, Defect1 = Defect};
	 nl, nl,
	 efficiency_text(Trace, Defect, Defect1)).
trace_text(Trace, Defect, Defect1) -->
	['Glückwunsch, Ihre Lösung scheint korrekt zu sein!'],
	nl, efficiency_text(Trace, Defect, Defect1).


efficiency_text(Trace, Defect, Defect) -->
	['Schauen wir uns nun die Effizienzdaten an:'],
	{member(trace(space, SpaceRatio, SpaceEF), Trace),
	 member(trace(time, TimeRatio, TimeEF), Trace)},
	nl, ['Ihre Lösung benötigt '],
	efficiency_compare(TimeRatio), [' Zeit wie die Musterlösung. '],
	efficiency_result(TimeEF),
	nl, ['An Speicher verbrauchen Sie '],
	efficiency_adverb(SpaceRatio, TimeRatio),
	efficiency_compare(SpaceRatio), ['. '],
	efficiency_result(SpaceEF).

efficiency_compare(1) --> !,
	['genau soviel'].
efficiency_compare(Ratio) -->
	[Ratio, 'mal soviel'].

efficiency_result(EF) -->
	{EF > 1, !,
	 Percent is (EF - 1) * 100},
	nl, ['Das verbessert Ihr Ergebnis um ', Percent, ' Prozent.'].
efficiency_result(EF) -->
	{EF < 1, !,
	 Percent is (1 - EF) * 100},
	nl, ['Das würde Sie ', Percent, ' Prozent Ihrer verbliebenen Punktzahl'],
	nl, ['kosten.'].
efficiency_result(_) --> [].

efficiency_adverb(Ratio1, Ratio2) -->
	{S is sign(Ratio1),
	 S is sign(Ratio2)}, !.
efficiency_adverb(_, _) -->
	['dagegen '].





%%%%% Direkte Modifikation des globalen Zustands




%%% replace_UH(+UH, +State1, -State2)

replace_UH(UH, [IS, RR, Exm, [Unm, Fnm, UP, _]], [IS, RR, Exm, [Unm, Fnm, UP, UH]]) :-
	retract(user(Unm, Pwd, Fnm, UP, _)),
	assert(user(Unm, Pwd, Fnm, UP, UH)),
	update_user_file.





%%%%% Modifikation von Dateien




%%% update_user_file

update_user_file :-
	tell('users.pl'),
	write('%%% user(+Username, ?Password, -Forename, -Userpath_as_atom, -UserHistory)'), nl,
	write('% Liefert Angaben über die Benutzer des Systems.'), nl,
	write('% Die Benutzergeschichte (UserHistory) ist mit einer'),
	write(' leeren Liste zu initialisieren.'), nl,
	listing(user/5),
	told.





%%%%% Testprädikate:

/*

lf(Name) :-
	load_file(Name, _, _).


se(Name, Tag) :-
	user(jan, _, ForeName, Path, UH),
	load_exemplaries(Exm),
	read_file(Name, RR),
	see_exercise(Tag, [_, RR, Exm, [jan, ForeName, Path, UH]], _State).

gh(Name, Tag, State) :-
	user(jan, _, ForeName, Path, _UH),
	load_exemplaries(Exm),
	read_file(Name, RR),
	(HintInfo = []; HintInfo = [[levels(0), 1]]),
	give_hint([[exercise, Tag, _], RR, Exm,
			[jan, ForeName, Path, [[1.1, 5, HintInfo, 0.7]]]], State).

shr :-
	user(jan, _, _, _, UH),
	load_exemplaries(Exm),
	show_results([_, _, Exm, [_, _, _, UH]]).


*/