%%%%%% Parameter






:- ensure_loaded('env_b10.pl').




%%%% Bewertung von Aufgabenl�sungen



%%% time_eval_factor(TimeRatio, Time_evaluation_factor)
% Liefert zum Verh�ltnis des Zeitbedarfs der Musterl�sung zu dem der Sch�lerl�sung
% den Faktor, mit dem die f�r die betreffende Aufgabe zu vergebende Punktzahl
% zu multiplizieren ist (den sog. "Evaluierungsfaktor").

time_eval_factor(TimeRatio, TimeEF) :-
	TimeRatio >= 1, !,
	TimeEF is 1 / (log10(TimeRatio + 1) - log10(2) + 1).
	
% Bei Belohnungen sind wir etwas gro�z�giger und verwenden den nat�rlichen Logarithmus:
time_eval_factor(TimeRatio, TimeEF) :-
	TimeEF is log(1 / TimeRatio + 1) - log(2) + 1.


%%% space_eval_factor(SpaceRatio, Space_evaluation_factor)
% Liefert den Evaluierungsfaktor f�r das Verh�ltnis des Speicherbedarfs der Musterl�sung
% zu dem der Sch�lerl�sung.

space_eval_factor(SpaceRatio, SpaceEF) :-
	SpaceRatio >= 1, !,
	SpaceEF is 1 / (log10(SpaceRatio + 1) - log10(2) + 1).
	
% Bei Belohnungen sind wir etwas gro�z�giger und verwenden den nat�rlichen Logarithmus:
space_eval_factor(SpaceRatio, SpaceEF) :-
	SpaceEF is log(1 / SpaceRatio + 1) - log(2) + 1.

	
%%% correctness_eval_factor(CorrectnessRatio, Correctness_evaluation_factor)
% Liefert den Evaluierungsfaktor f�r den Anteil der Testaufrufe der Sch�lerfunktion, 
% die ein falsches Resultat lieferten.

correctness_eval_factor(Correctness, Correctness).


%%% operators_hint_eval_factor(EF)
% Liefert den Evaluierungsfaktor f�r Hinweise, die in der Auflistung der in
% der jeweiligen Musterl�sung verwendeten Operatoren bestehen.

operators_hint_eval_factor(0.92).




%%%% Direktiven



%%% directives_marker(+Char)
% Definiert das Zeichen, das Direktiven in einem Lisp-Programm einleitet
% (etwa Aufgabenmarkierungen).

directives_marker(92). %(\)


exercise_marker('A').

repeat_marker('R').

function_marker('F').

equivalence_marker('E').

points_marker('P').

test_parameters_marker(test).




%%%% Einlesen von Lisp-Programmen



%%% tablength(-N)

tablength(8).




%%%% Ausgabe



%%% shown_counter_examples(-N)
% Bestimmt die Anzahl der gezeigten Gegenbeispiele, wenn Sch�ler- und Musterl�sung
% nicht �quivalent sind.

shown_counter_examples(10).




%%%% Namen von Datendateien



errorfile_name(Username, FileName) :-
	tempfile_name(error_, Username, FileName).


tracefile_name(Username, FileName) :-
	tempfile_name(trace_, Username, FileName).


opsfile_name(Username, FileName) :-
	tempfile_name(ops_, Username, FileName).


evalfile_name(Username, FileName) :-
	tempfile_name(eval_, Username, FileName).


tempfile_name(Part1, Username, FileName) :-
	concat_atom([Part1, Username], RelativeFN),
	name(RelativeFN, RelativeFN1),
	tmp_path(Path, RelativeFN1),
	name(FileName, Path).
