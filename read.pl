%%%%%% Repr�sentation von Lisp-Programmen

 
 
 
 
:- ensure_loaded('parameters.pl').
:- ensure_loaded('auxprd.pl').




% Das vorliegende Programm geht von folgendem Aufbau eines Lisp-Programms aus. 
% Dabei ist es gleichg�ltig, ob es sich um ein Sch�lerprogramm oder eine Musterl�sungsdatei
% handelt (ein Sch�lerprogramm enth�lt allerdings typischerweise keine Direktiven):
%	Definitionen f�r Hilfsfunktionen, allgemeine Direktiven und Kommentare
%	Aufgabenmarkierung f�r die 1. Aufgabe
%	Aufgabenspezifische Direktiven und Musterl�sung
%	Aufgabenmarkierung f�r die 2. Aufgabe
%	Aufgabenspezifische Direktiven und Musterl�sung
%	usw.
%
% Die Repr�sentation einer solchen Datei sieht wie folgt aus:
%	[Dateiname, allg. Informationen zum Ablauf des Leseprozesses,
%		[0, allg. Direktiven, Programmzeilen],
%		[Aufgabenmarkierung 1, Direktiven, Programmzeilen],
%		...,
%		[Aufgabenmarkierung n, Direktiven, [Programmzeilen..., Endzeile],
%     ]
% Eine Programmzeile ist wie folgt repr�sentiert: 
%	[Zeilennummer, Anfangs-Ebene| Zeichen und Objektrepr�sentationen]
% Die End-Zeile ist wie folgt repr�sentiert:
%	[Letzte Zeilennummer + 1, Anfangs-Ebene, Datei-Ende]
%
% Die Anfangs-Ebene einer Zeile ist die Klammerebene, auf der die Zeile beginnt (zu unter-
% scheiden von ihrer Layout-Ebene).
%
% Die allgemeine Form von Objektrepr�sentationen ist [A, B, C], wobei:
% A = Atom zur Bezeichnung des Objekt-Typs,
% B = Genauere Beschreibung des Objekts (in Form irgendeiner Prolog-Struktur),
% C = Kontext.
%
% F�r folgende Objekte gibt es spezielle Repr�sentationen: 
%	Durch '|' begrenzte Symbole: repr�sentiert als [symbol, Symbol-Name, Kontext]
%  	Andere Symbole: repr�sentiert als [sym, Symbol-Name, Kontext]
%  	Strings: repr�sentiert als [string, String, Kontext]
% 	Unbekannte Read-Makros mit vermutlich flacher Struktur: 
%		repr�sentiert als [rm_x, Prolog-Atom, Kontext]
%  	Vektoren, die durch #<Ziffern>(...) dargestellt sind: 
% 		repr�sentiert wie Listen als Zeichensequenz, allerdings 
% 		mit vorangestelltem [rm_n, <Ziffern>, Kontext].
%	Unbekannte Read-Makros mit vermutlich listenartiger Struktur, dargestellt durch
%		#<Zeichen>(...): 
%		repr�sentiert wie Listen als Zeichensequenz, allerdings
%		mit vorangestelltem [rm_x, <Zeichen>, Kontext].
%	Unbekannte Read-Makros mit vermutlich listenartiger Struktur, dargestellt durch
%		#<Ziffern><Zeichen>(...):
%		repr�sentiert wie Listen als Zeichensequenz, allerdings
%		mit vorangestelltem [rm_n, <Ziffern>, Kontext], [rm_x, <Zeichen>, Kontext].
%	Datei-Ende: repr�sentiert als [end, '', Kontext]
%
% Zur Repr�sentation des jeweiligen Kontexts s. readf.


%%% read_file(-Name, +Read_representation)
% Liest ein Lisp-Programm ein und gibt dessen Repr�sentation sowie Zusatzinformationen,
% etwa �ber dabei aufgetretene Fehler, zur�ck.

read_file(Name, [Name, RPI| Program]) :-
	access_file(Name, read), !,
	see(Name),
	Line = 1, Level = 0,
	readf([t], Line, 0, Level, [0], [[[[Level, Line]], [], 0]], Program_r, [], RPI_r),
	seen, !,
	reverse_all(Program_r, Program),
	reverse(RPI_r, RPI).
read_file(Name, [Name, [file_error(no_read_access, Name)]]) :-
	access_file(Name, exist), !.
read_file(Name, [Name, [file_error(does_not_exist, Name)]]).


%%% read_object(-Object_read_representation, -Read_process_information)
% Liest ein einzelnes Lisp-Objekt oder, wenn das n�chste Zeichen nicht der Beginn eines
% solchen Objektes ist, nur dieses Zeichen und liefert eine Repr�sentation des
% entsprechenden Programmabschnitts.
% Ausnahmen sind Zeilenkommentar (wird wie ein Zeilenumbruch behandelt), Blockkommentar
% (wird wie ein eigenes Lisp-Objekt behandelt) und Aufgabenmarkierungen (werden
% wie das Ende einer Aufgabe behandelt; die erste Zeile der neuen Aufgabe wird aber
% nicht mitrepr�sentiert).

read_object(Object, Information) :-
	Line = 1, Level = 0,
	readf([end_], Line, 0, Level, [0], [[[[Level, Line]], [], 0]], Object_r, [], Info),
	reverse_all(Object_r, [[_, _, Object]| _]),
	reverse(Info, Information).



%%% readf(+State, +Line, +Level, +Position, +Program, -Result)
% Entscheidendes Hilfspr�dikat von read_file und read_object.
% Allg. Funktionsweise: readf liest bei jedem Aufruf ein Zeichen ein und gibt
% dieses an sein Hilfspr�dikat readf_ weiter, das dann entscheidet, wie es weiter geht, d.h.
% insbesondere, in welchen Zustand der Einleseprozess wechselt.
% Wenn das n�chste Zeichen eingelesen werden soll, ruft readf_ wieder readf auf.
% Da der Leseprozesses seriell ist, die zu lesenden Strukturen aber rekursiv verschachtelt,
% ist der Zustand des Prozesses als Stack organisiert. Was die konkret verwendeten Elemente
% dieses Stacks bedeuten, ist im folgenden zusammengefa�t:
%  [cdr_| _]     : in einer Liste (evtl. einer 'dotted list') wurde gerade ein Punkt gelesen
%  [dot, _]      : es wird gerade eine Reihe von Punkten gelesen, die nur als Anfang eines
%                   Symbolnamens nicht zu einem Fehler f�hrt (s. Text im Pr�dikat dot_error)
%  [rm_n| _]     : ein Read-Makro-Ausdruck der Form "#n..." (wobei n eine Ziffernfolge ist)
%                   wird gelesen
%  [rm_x| _]     : ein Read-Makro-Ausdruck unbekannter Art, aber mit vermutlich flacher 
%                   Struktur wird gelesen (z.B. "#:foo")
%  [string, _, _]: ein String wird gelesen
%  [sym, _]      : ein normales Symbol wird gelesen
%  [symbol, _, _]: ein von "|" begrenztes Symbol wird gelesen
%  bq            : der gerade zu lesende Ausdruck befindet sich im Bereich eines 'Backquotes'
%  ca_at         : der gerade zu lesende Ausdruck bef. s. im Bereich eines 'Komma-At'
%                   entsprechend der Backquote-Syntax     
%  ca_dot        : der gerade zu lesende Ausdruck bef. s. im Bereich eines 'Komma-Punktes'
%                   entsprechend der Backquote-Syntax
%  char          : der Name eines Zeichens w. gel., entsprechend dem Read-Makro "#\"
%  cdr           : der Leseprozess befindet sich im cdr-Teil einer 'dotted list'
%  comma         : der gerade zu lesende Ausdruck bef. s. im Bereich eines 'Kommas'
%                   entsprechend der Backquote-Syntax
%  comma_        : es ist gerade ein Komma in einem entsprechend mit Backquote(s) versehenen
%                   Ausdruck gelesen worden
%  comment       : der Leseproze� befindet sich in einem Blockkommentar
%  d([D| Args])  : eine Direktive D mit den Argumenten Args ist auszuf�hren
%  end           : markiert den Top-Level des Leseprozesses und beendet diesen
%  end_          : der Leseproze� wird nach dem Lesen eines einzelnen weiteren Objekts (oder 
%                   eines Zeichens, das nicht den Beginn eines Objekts markiert) beendet
%  function      : der gerade zu lesende Ausdruck bef. s. im Bereich eines 'Sharp-Quote' ("#'")
%  l             : eine Liste wird gelesen
%  new_line      : es ist ein Zeilenumbruch erreicht und eine neue Zeile ist in
%                   die Programmrepr�sentation einzuf�gen 
%  quote         : der gerade zu lesende Ausdruck bef. s. im Bereich eines 'Quotes'
%  rm            : das einen Read-Makro-Ausdruck einleitende "#" wurde gerade gelesen
%  rm_xl         : ein Read-Makro-Ausdruck unbekannter Art, aber mit vermutlich rekursiver
%                   Struktur w. gel. (z.B. "#S(foo)")
%  t             : markiert den Top-Level des Leseprozesses 
%  vec           : ein Vector w. gel., entsprechend dem Read-Makro "#(...)" bzw. "#n(...)"


readf([end], _, _, _, _, Object, Object, Info, Info) :- !.
readf(State, Line, Char, Level, Pos, Program, Result, Info, NewInfo) :- 
	get0(C), 
	Char1 is Char+1,
	(State = [end_], !, State1 = [end]; 
	 State1 = State),
	readf_(State1, C, Line, Char1, Level, Pos, Program, Result, Info, NewInfo).


%%% readf_(+State, +NewChar, +Line, +CharPos, +Level, +TokenPos, +Program, -Result, 
%%%	+Info, -NewInfo)
% Entscheidendes Hilfspr�dikat von readf. Info und NewInfo speichern zus�tzliche Informationen
% zum gelesenen Programm (z.Z. nur, wo sich �berz�hlige schlie�ende Klammern befinden)

%%% H�ufige F�lle

%% Leerzeichen und Zeilenumbr�che

% Eine neue Zeilenrepr�sentation in die Repr�sentation der Aufgabe einf�gen:
readf_([new_line| S], _, L, _, Level, Pos, [[Lines, Dir, E]| PR], Result, Info, NewInfo) :-
	!,
	trim_first(Lines, Lines1),
	N is L+1,
	readf(S, N, 0, Level, Pos, [[[[Level, N]| Lines1], Dir, E]| PR], Result, Info,
		NewInfo).

% Lesen eines Zeilenumbruchs (der eventuell ein Symbol abschlie�t)
readf_([X| S], C, L, _, Level, Pos, Program, Result, Info, NewInfo) :-
	newline(C), (code_like(X); X=[sym, _]), !,
	get_symbol([X| S], Program, S1, Program1),
	remove_quotes(S1, S2),
	readf_([new_line| S2], C, L, _, Level, Pos, Program1, Result, Info, NewInfo).

% Behandlung von Carriage returns
readf_([X| S], C, L, _, Level, Pos, Program, Result, Info, NewInfo) :-
	carriage_return(C), code_like(X), !,
        readf([X| S], L, 0, Level, Pos, Program, Result, Info, NewInfo).

% Leerzeichen im allgemeinen ("code_like") Kontext
readf_([X| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	code_like(X), space(C), !,
	readf([X| S], L, CP, Level, Pos, [[[[C| Line]| LR], Dir, E]| PR], Result, Info, NewInfo).

% Tabs im allgemeinen ("code_like") Kontext
readf_([X| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	code_like(X), htab(C), !,
	tablength(N), space(Sp),
	mk_list(N, Sp, Spaces),
	append(Spaces, Line, Line1),
	readf([X| S], L, CP, Level, Pos, [[[Line1| LR], Dir, E]| PR], Result,
		[tabs(L, N)| Info], NewInfo).



%% Symbole

% Beginn des Lesens eines Symbols
readf_([X| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :-
	symbol_start(C, C1), \+ dot(C), code_like(X), !,
	(newline(C1), !,
	 get_symbol([[sym, [C1]], X| S], Program, S1, Program1),
	 readf_([new_line, [sym, []]| S1], C1, L, CP, Level, Pos, Program1,
	 	Result, Info, NewInfo);
 	 readf([[sym, [C1]], X| S], L, CP, Level, Pos, Program, Result, Info, NewInfo)).

% Symbol-Zeichen tempor�r im Zustand speichern
readf_([[X, W]| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :-
	sym_like([X, W]), symbol_char(C, C1), \+ colon(C), !,
	(newline(C1), !,
	 get_symbol([[X, [C1| W]]| S], Program, S1, Program1),
	 readf_([new_line, [X, []]| S1], C1, L, CP, Level, Pos, Program1, Result, Info, NewInfo);
	 readf([[X, [C1| W]]| S], L, CP, Level, Pos, Program, Result, Info, NewInfo)).

% Doppelpunkte einlesen
readf_([[X, [WH| WR]]| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :-
	colon(C), sym_like([X, [WH| WR]]),
	\+ member(C, WR), !,
 	readf([[X, [C, WH| WR]]| S], L, CP, Level, Pos, Program, Result, Info, NewInfo).

% Zu viele ungesch�tzte Doppelpunkte, oder an zu vielen Stellen
readf_([[X, [WH| WT]]| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :-
	colon(C), sym_like([X, [WH| WT]]), !,
	readf([[X, [C, WH| WT]]| S], L, CP, Level, Pos, Program, Result,
		[error(L, CP, colon)| Info], NewInfo).

% Abschlu� eines Symbols
readf_([X| S], C, L, CP, Level, [Pos| PosR], Program, Result, Info, NewInfo) :-
	sym_like(X),
	(symbol_terminator(C),	Pos1 is Pos+1;
	 escape_char(C), Pos1 is Pos),
	\+ (X=[rm_x| _], opening_parenthesis(C)), !,
	get_symbol([X| S], Program, S1, Program1),
	remove_quotes(S1, S2),
	readf_(S2, C, L, CP, Level, [Pos1| PosR], Program1, Result, Info, NewInfo).


%% Listen 

% Beginn einer Liste
readf_([X| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :- 
	opening_parenthesis(C),
	code_like(X), !,
	Level1 is Level+1,
	readf([l, X| S], L, CP, Level1, [0| Pos], [[[[C| Line]| LR], Dir, E]| PR], 
		Result, Info, NewInfo).

% Abfangen unerlaubt beendeter dotted lists
readf_([cdr| S], C, L, CP, Level, [Pos0, Pos1| PosR], [[[Line| LR], Dir, E]| PR],
		Result, Info, NewInfo) :- 
	closing_parenthesis(C), 
	Pos0 \= 1, !,
	remove_quotes(S, S1),
	Level1 is Level - 1,	
	Pos1_ is Pos1 + 1,
	readf(S1, L, CP, Level1, [Pos1_| PosR], [[[[C| Line]| LR], Dir, E]| PR], Result, 
		[error(L, CP, dotted_list)| Info], NewInfo).
	
% Abschlu� eines listen�hnlichen Konstrukts durch eine schlie�ende Klammer:
readf_([X| S], C, L, CP, Level, [_, Pos| PosR],  [[[Line| LR], Dir, E]| PR],
		Result, Info, NewInfo) :-
	closing_parenthesis(C),
	list_like(X), !,
	remove_quotes(S, S1),
	Level1 is Level - 1,	
	Pos1 is Pos + 1,
	readf(S1, L, CP, Level1, [Pos1| PosR], [[[[C| Line]| LR], Dir, E]| PR],
		Result, Info, NewInfo).


%% Direktiven

% Eine neue Aufgabenrepr�sentation in die Repr�sentation des Programms einf�gen:
readf_([d([X, E| _])| S], _, L, CP, Level, Pos, Program, Result, Info, NewInfo) :-
	exercise_marker(X), !,
        readf_([new_line| S], 0, L, CP, Level, Pos, [[[], [], E]| Program],
		Result, Info, NewInfo). 	

% Nicht f�r das Einlesen relevante Direktiven in der daf�r vorgesehenen Liste 
% innerhalb der Aufgabenrepr�sentation sammeln:
readf_([d(D)| S], _, L, CP, Level, Pos, [[Lines, Dir, E]| R], Result, Info, NewInfo) :- !,
	readf_([new_line| S], 0, L, CP, Level, Pos, [[Lines, [d(D)| Dir], E]| R], 
		Result, Info, NewInfo).

% Direktiven erkennen und in den Zustand des Einleseprozesses �bernehmen:
readf_(S, C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	semicolon(C), directive(D), !,
	readf_([d(D)| S], 0, L, CP, Level, Pos, Program, Result, Info, NewInfo).


%% Ignorieren von Zeilenkommentar:

readf_(S, C, L, _, Level, Pos, Program, Result, Info, NewInfo) :- 
	semicolon(C), 
	\+ S = [comment| _], !, 
	line(_),
	readf_([new_line| S], 0, L, _, Level, Pos, Program, Result, Info, NewInfo).


%%% Seltenere F�lle

%% Quote und Backquote (und was damit zusammenh�ngt)

readf_([X| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	quote(C), code_like(X), !,
	readf([quote, X| S], L, CP, Level, Pos, [[[[C| Line]| LR], Dir, E]| PR], 
		Result, Info, NewInfo).

readf_([X| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	backquote(C), code_like(X), !,
	readf([bq, X| S], L, CP, Level, Pos, [[[[C| Line]| LR], Dir, E]| PR], 
		Result, Info, NewInfo). 

readf_([comma_| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	dot(C), remove_quotes(S, [l| _]), !,
	comma(C1),
	readf([ca_dot| S], L, CP, Level, Pos, [[[[C, C1| Line]| LR], Dir, E]| PR], 
		Result, Info, NewInfo). 

readf_([comma_| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	dot(C), !,
	comma(C1),
	readf([ca_dot| S], L, CP, Level, Pos, [[[[C, C1| Line]| LR], Dir, E]| PR], Result, 
		[error(L, CP, comma(',.'))| Info], NewInfo). 
		
readf_([comma_| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	at_sign(C), remove_quotes(S, [l| _]), !,
	comma(C1),
	readf([ca_at| S], L, CP, Level, Pos, [[[[C, C1| Line]| LR], Dir, E]| PR],
		Result, Info, NewInfo). 

readf_([comma_| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	at_sign(C), !,
	comma(C1),
	readf([ca_at| S], L, CP, Level, Pos, [[[[C, C1| Line]| LR], Dir, E]| PR], Result, 
		[error(L, CP, comma(',@'))| Info], NewInfo). 

readf_([comma_| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	!,
	comma(C1),
	readf_([comma| S], C, L, CP, Level, Pos, [[[[C1| Line]| LR], Dir, E]| PR], 
		Result, Info, NewInfo). 

readf_([X| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :-
	comma(C), (code_like(X); sym_like(X)), 
	findall(X1, (member(X1, [X| S]), X1=bq), BQs), 
	findall(X1, (member(X1, [X| S]), comma_like(X1)), Commas),
	length(BQs, L1), length(Commas, L2), L1>L2, !,
	readf([comma_, X| S], L, CP, Level, Pos, Program, Result, Info, NewInfo).

% Abfangen von Komma in unzul�ssigem Kontext:
readf_([X| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :-
	comma(C), (code_like(X); sym_like(X)), !,
	readf([comma_, X| S], L, CP, Level, Pos, Program, Result, 
		[error(L, CP, no_backquote)| Info], NewInfo).


%% Strings und durch "|" eingeschlossene Symbole

% Anfang
readf_([X| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	delimiter(C, Y), code_like(X), !, 
	readf([[Y, false, []], X| S], L, CP, Level, Pos, 
		[[[[C| Line]| LR], Dir, E]| PR], Result, Info, NewInfo).

% Lesen eines Zeilenumbruchs
readf_([[X, _, W]| S], C, L, _, Level, Pos, [[[Line| LR], Dir, E]| PR], 
		Result, Info, NewInfo) :- 
	newline(C), !,
	reverse([C| W], W1), name(Nm, W1),
	readf_([new_line, [X, false, []]| S], C, L, _, Level, Pos,
		[[[[[c(S), Nm, X]| Line]| LR], Dir, E]| PR], Result, Info, NewInfo).

% Abschlu� von Strings
readf_([[string, false, W]| S], C, L, CP, Level, [Pos| PosR], 
		[[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :- 
	delimiter(C, string), !,
	reverse(W, W1), name(Nm, W1),	
	remove_quotes(S, S1),
	Pos1 is Pos+1,
	readf(S1, L, CP, Level, [Pos1| PosR], 
		[[[[C, [c(S), Nm, string]| Line]| LR], Dir, E]| PR], Result, Info, NewInfo).

% Abschlu� von |Symbolen|
readf_([[symbol, false, W]| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	delimiter(C, symbol), !,
	get_symbol([[symbol, W]| S], Program, S1, [[[Line| LR], Dir, E]| PR]),
	readf([[sym, []]| S1], L, CP, Level, Pos, [[[[C| Line]| LR], Dir, E]| PR],
		Result, Info, NewInfo).

% Lesen des Escape-Zeichens
readf_([[X, false, W]| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	backslash(C), !,
	readf([[X, true, [C| W]]| S], L, CP, Level, Pos, Program, Result, Info, NewInfo).
	
% Einlesen der Zeichen, die zum String bzw. Symbol geh�ren
readf_([[X, _, W]| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	\+ end_of_file(C), !,
	readf([[X, false, [C| W]]| S], L, CP, Level, Pos, Program, Result, Info, NewInfo).


%% Durch "#" eingeleitete Read-Makros (allgemein)

readf_([X| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :- 
	hash_sign(C), code_like(X), !,
	readf([rm, X| S], L, CP, Level, Pos, [[[[C| Line]| LR], Dir, E]| PR], 
		Result, Info, NewInfo).

% �bergang zu Block-Kommentar. Zum Zweck einer nachtr�glichen Layout-Kritik wird noch
% ein Vermerk [comment] in die Programmrepr�sentation eingetragen.
readf_([rm| S], C, L, CP, Level, Pos, [[[[_| Line]| LR], Dir, E]| PR], Result, 
		Info, NewInfo) :- 
	vert_bar(C), !,
	readf([comment| S], L, CP, Level, Pos, [[[[[comment]| Line]| LR], Dir, E]| PR], 
		Result, Info, NewInfo).

% Zeichen ("#\")
readf_([rm| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :- 
	backslash(C), !,
	get0(C1),
	CP1 is CP+1,
	(newline(C1), !,
	 get_symbol([[char, [C1]]| S], [[[[C| Line]| LR], Dir, E]| PR], S1, Program1),
	 readf_([new_line, [char, []]| S1], C1, L, CP1, Level, Pos, Program1,
	 	Result, Info, NewInfo);

	 end_of_file(C1), !,
	 readf_([rm| S], C1, L, CP, Level, Pos, [[[[C| Line]| LR], Dir, E]| PR], Result,
	 	[error(L, CP, escape_char)| Info], NewInfo);

	 readf([[char, [C1]]| S], L, CP1, Level, Pos, [[[[C| Line]| LR], Dir, E]| PR],
	 	Result, Info, NewInfo)).

% 'Sharp-Quote' ("#'")
readf_([rm| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	quote(C), !,
	readf([function| S], L, CP, Level, Pos, [[[[C| Line]| LR], Dir, E]| PR],
		Result, Info, NewInfo).

% �bergang zu Vektoren (repr�sentiert als "#(...)")
readf_([rm| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	opening_parenthesis(C), !,
	Level1 is Level+1,
	readf([vec| S], L, CP, Level1, [0| Pos], [[[[C| Line]| LR], Dir, E]| PR],
		Result, Info, NewInfo).

% �bergang zu Vektoren (repr�sentiert als "#n(...)", wobei n eine Ziffernfolge ist)
readf_([[rm_n, W]| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR],
		Result, Info, NewInfo) :-
	opening_parenthesis(C), !,
	Level1 is Level+1,
	reverse(W, W1),
	name(N, W1),
	readf([vec| S], L, CP, Level1, [0| Pos], 
		[[[[C, [S, N, rm_n]| Line]| LR], Dir, E]| PR], Result, Info, NewInfo).

% Behandlung von Read-Makro-Ausdr�cken der Form #n... (wobei n eine Ziffernfolge ist)
readf_([X| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	(X=rm, W=[]; X=[rm_n, W]),
	digit(C), !,
	readf([[rm_n, [C| W]]| S], L, CP, Level, Pos, Program, Result, Info, NewInfo).

% �bergang zu unbekannten, durch #n eingeleiteten Read-Makros
readf_([[rm_n, W]| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], 
		Result, Info, NewInfo) :- 
	\+ impossible_dispatch_macro_character(C), !,
	reverse(W, W1),
	name(N, W1),
	readf([[rm_x, [C]]| S], L, CP, Level, Pos,
		[[[[[S, N, rm_n]| Line]| LR], Dir, E]| PR], Result, Info, NewInfo).

% �bergang zu unbekannten, durch # eingeleiteten Read-Makros
readf_([rm| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	\+ impossible_dispatch_macro_character(C), !,
	readf([[rm_x, [C]]| S], L, CP, Level, Pos, Program, Result, Info, NewInfo).

% �bergang zu einem unbekannten Read-Makro-Ausdruck mit vermutlich listenartiger Struktur
readf_([[rm_x, W] | S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], 
		Result, Info, NewInfo) :- 
	opening_parenthesis(C), !,
	Level1 is Level+1,
	reverse(W, W1),
	name(Nm, W1),
	readf([rm_xl| S], L, CP, Level1, [0| Pos], 
		[[[[C, [S, Nm, rm_x]| Line]| LR], Dir, E]| PR], Result, Info, NewInfo).


%% Block-Kommentar

% Ignorieren des Kommentars
readf_([comment| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	\+ end_of_file(C), \+ vert_bar(C), !,
	readf([comment| S], L, CP, Level, Pos, Program, Result, Info, NewInfo).

% Erstes Zeichen des Abschlu�-Tokens: |
readf_([comment| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	vert_bar(C), !,
	readf([comment_| S], L, CP, Level, Pos, Program, Result, Info, NewInfo).

% Eigentlicher Abschlu� des Kommentars. Zum Zweck einer nachtr�glichen Layout-Kritik wird noch
% ein Vermerk [comment] in die Programmrepr�sentation eingetragen.
readf_([comment_| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], 
		Result, Info, NewInfo) :- 
	hash_sign(C), !,
	readf(S, L, CP, Level, Pos, [[[[[comment]| Line]| LR], Dir, E]| PR],
		Result, Info, NewInfo).

readf_([comment_| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :-
	!,
	readf_([comment| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo).


%% Punkte

% Punkt in einer Liste
readf_([l| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	dot(C), !,
	readf([[cdr_, [C]], l| S], L, CP, Level, Pos, Program, Result, Info, NewInfo).

% Punkt nach einem Punkt
readf_([[X, W]| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	dot_like([X, W]), dot(C), !,
	readf([[dot, [C| W]]| S], L, CP, Level, Pos, Program, Result, Info, NewInfo).

% Punkt in einem anderen Kontext
readf_([X| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	code_like(X), dot(C), !,
	readf([[dot, [C]], X| S], L, CP, Level, Pos, Program, Result, Info, NewInfo).

% Lesen eines mit einem oder mehreren Punkten beginnenden Symbols
readf_([[X, W]| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :-
	dot_like([X, W]), symbol_char(C, C1), \+ dot(C), !,
	(newline(C1), !,
	 get_symbol([[sym, [C1| W]]| S], Program, S1, Program1),
	 readf_([new_line, [sym, []]| S1], C1, L, CP, Level, Pos, Program1, 
	 	Result, Info, NewInfo); 
 	 readf([[sym, [C1| W]]| S], L, CP, Level, Pos, Program, Result, Info, NewInfo)).

% Lesen eines mit einem oder mehreren Punkten beginnenden Symbols, das im Code in 
% der Form ..|foo| repr�sentiert ist
readf_([[X, W]| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :-
	dot_like([X, W]), (delimiter(C, symbol); escape_char(C)), !,
	get_symbol([[sym, W]| S], Program, S1, Program1),
      readf_(S1, C, L, CP, Level, Pos, Program1, Result, Info, NewInfo).

% �bergang zum cdr-Teil in einer dotted list
readf_([[cdr_| _], l| S], C, L, CP, Level, [Pos| PosR], [[[Line| LR], Dir, E]| PR], 
		Result, Info, NewInfo) :-
	Pos>0, % da Listen auch einen car-Teil brauchen
	symbol_terminator(C), !,
	dot(C1),
	readf_([cdr| S], C, L, CP, Level, [-1| PosR] , [[[[C1| Line]| LR], Dir, E]| PR], 
		Result, Info, NewInfo).

% Reine Punkt-Reihen abfangen
readf_([[dot| _], X| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	code_like(X), !,
	readf_([X| S], C, L, CP, Level, Pos, Program, Result, 
		[error(L, CP, dot)| Info], NewInfo).

% Abfangen von "dotted lists" ohne car-Teil (dennoch �ergang zum cdr-Teil, aber Ablegen
% einer Fehlerrepr�sentation in Info)
readf_([[cdr_| _], l| S], C, L, CP, Level, [Pos| PosR], [[[Line| LR], Dir, E]| PR], 
		Result, Info, NewInfo) :-
	Pos<1, symbol_terminator(C), !,
	dot(C1),
	readf_([cdr| S], C, L, CP, Level, [-1| PosR] , [[[[C1| Line]| LR], Dir, E]| PR], Result, 
		[error(L, CP, no_car)| Info], NewInfo).



%% Das Escape-Zeichen

readf_([X| S], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	escape_char(C), (code_like(X); dot_like(X)), !,
	get0(C1),
	CP1 is CP+1,
	(end_of_file(C1), !,
	 readf_([X| S], C1, L, CP, Level, Pos, [[[[C| Line]| LR], Dir, E]| PR], Result,
	 	[error(L, CP, escape_char)| Info], NewInfo);
         readf_([X| S], [C1], L, CP1, Level, Pos, [[[[C| Line]| LR], Dir, E]| PR],
	 	Result, Info, NewInfo)).


%% Rekursionsverankerung: EOF

readf_([X| S], C, L, CP, Level, _, [[Lines, Dir, E]| Program],
		[[[[[c([X| S]), '', end], Level, L1]| Lines1], Dir, E]| Program],
		Info, Info1) :-
	end_of_file(C), !,
	trim_first(Lines, Lines1),
	L1 is L+1,
	(top(X), !,
	 Info1 = Info;
	 Info1 = [error(L, CP, end(X))| Info]).


%% Abfangen von letzten Fehlern und unerwarteten F�llen

% Abfangen von #, gefolgt von Newline
readf_([X| S], C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	(X=rm; X=[rm_n| _]), 
	newline(C), !,
	remove_quotes(S, S1),
	readf_([new_line| S1], C, L, _, Level, Pos, Program, Result,
		[error(L, CP, hash_sign)| Info], NewInfo).

% Abfangen anderer freistehender #-Zeichen
readf_([X| S], _, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	(X=rm; X=[rm_n| _]), !,
	readf([X| S], L, CP, Level, Pos, Program, Result,
		[error(L, CP, hash_sign)| Info], NewInfo).

% Behandlung �berz�hliger schlie�ender Klammern:
readf_([X], C, L, CP, Level, Pos, [[[Line| LR], Dir, E]| PR], Result, Info, NewInfo) :-
	closing_parenthesis(C), 
	top(X), !,
	Level1 is Level - 1, 
	readf([X], L, CP, Level1, Pos, [[[[C| Line]| LR], Dir, E]| PR], Result, Info, NewInfo).

% Abfangen unzul�ssiger schlie�ender Klammern:
readf_(S, C, L, CP, Level, Pos, Program, Result, Info, NewInfo) :- 
	closing_parenthesis(C), !, 
	readf(S, L, CP, Level, Pos, Program, Result,
		[error(L, CP, closing_parenthesis)| Info], NewInfo).

readf_(State, Char, Line, CP, _, _, Program, [error, Line, CP], Info, Info) :-
	nl, 
	write('State: '), write(State), nl,
	write('Program: '), write(Program), nl,
	write('Char: '), write(Char), nl.



%%% remove_quotes(+State, -State)
% Entfernt alle Elemente aus dem Zustands-Stack bis zum ersten nicht quote-�hnlichem Zustand.
% N�tzlich f�r den �bergang z.B. von [comma, quote, backquote, l| S], etwa bei "(`',a",
% zu [l], wenn als n�chstes Zeichen eine schlie�ende Klammer gelesen wird.

remove_quotes([X| S], [X| S]) :- \+ quote_like(X), !.
remove_quotes([_| R], S) :- 
	remove_quotes(R, S).


%%% get_symbol(+State, +Program, -State, -Program)
% Fa�t die ggf. im Zustand tempor�r gespeicherten Zeichen zu einem 'Symbol' zusammen und
% legt letzteres in der Programmrepr�sentation ab. Wenn der Zustand besagt, da� gerade
% kein 'Symbol' gelesen wird, geschieht nichts. 
% Hierbei ist unter 'Symbol' alles zu verstehen, was gerade eingelesen wird, wenn der
% Leseproze� sich in einem "sym_like" Zustand befindet, also nicht nur wirkliche Symbole,
% sondern auch Zeichensequenzen, die zu einem unbekannten Read-Makro geh�ren.
% Zusammen mit dem 'Symbol' wird auch dessen Kontext abgespeichert, und zwar verpackt als
% Argument eines c-Funktors, um ihn gegen das schlie�liche reverse_all zu sch�tzen.
% Das Pr�dikat wird dann aufgerufen, wenn eventuell gerade das Ende eines 'Symbols'
% erreicht worden ist, um Zustand und Programmrepr�sentation entsprechend zu aktualisieren.

get_symbol([[_, []]| S], P, S, P) :- !. % Dieser Fall kann eintreten bei Aneinanderreihungen
	% von normalen Symbol-Repr�sentationen und solchen, die durch "|" eingeschlossen sind.
get_symbol([[X, W]| S], [[[Line| LR], Dir, E]| PR], S,
 [[[[[c(S), Sym, X]| Line]| LR], Dir, E]| PR]) :- 
 	!, 
	reverse(W, W1),
	name(Sym, W1).
get_symbol(S, P, S, P).


%%% delimiter(+Char, -Atom)

delimiter(C, string) :- double_quote(C).
delimiter(C, symbol) :- vert_bar(C).


%%% directive(-Directive)

directive(I) :- 
	get0(C),
	directives_marker(C),
	line(I).



%%% trim_first(+Reversed_lines1, -Reversed_lines2)
% Wenn die erste Zeile von Reversed_lines (falls existent) nur aus Leerzeichen besteht, 
% werden diese im Ergebnis gel�scht. Ansonsten ist Reversed_lines1 mit Reversed_lines2 
% identisch.

trim_first([], []) :- !.
trim_first([Line| Lines], [Line1| Lines]) :-
	trimf(Line, Line, Line1).

trimf([Level, No], _, [Level, No]) :- !.
trimf([C| LR], Line, Result) :-
	\+ C = [_| _],
	\+ visible_char(C), !,
	trimf(LR, Line, Result).
trimf(_, Line, Line).



%%% append_end_information(LastLine


%%%% Zustandskategorien



%%% top(+State)
% Ein Zustand ist 'top', wenn er den Top-Level des Einleseprozesses markiert.

top(t).
top(end).


%%% list_like(+State)
% Ein Zustand ist "list_like", wenn in ihm beliebige Objekte und Leerzeichen akzeptiert 
% werden, aber der Zustand verlassen wird, wenn eine schlie�ende Klammer gelesen wird.

list_like(l).
list_like(cdr).
list_like(vec).
list_like(rm_xl).


%%% code_like(+State)
% Ein Zustand ist genau dann "code_like", wenn er nicht durch ein Leerzeichen beendet wird,
% und eine �ffnende Klammern eine Liste einleitet.

code_like(S) :- top(S), !.
code_like(S) :- list_like(S), !.
code_like(S) :- quote_like(S), !.


%%% quote_like(+State)
% Ein Zustand ist "quote_like", wenn nach ihm, nach beliebig vielen Leerzeichen und
% Zeilenumbr�chen, ein beliebiges Objekt stehen kann (bei "function" allerdings nur ein
% Symbol), und dieser Zustand nach dem Lesen dieses Objekts wieder verlassen wird.

quote_like(quote).
quote_like(bq).
quote_like(function).
quote_like(S) :- comma_like(S).


%%% comma_like(+State)

comma_like(comma).
comma_like(ca_at).
comma_like(ca_dot).


%%% sym_like(+State)
%% Ein Zustand ist "sym_like", wenn in ihm gerade eine Zeichensequenz eingelesen wird, die
% von einem 'Symbolterminator' beendet wird.

sym_like([sym, _]).
sym_like([char, _]).
sym_like([rm_x, _]).


%%% dot_like(+State)
% Ein Zustand ist "dot_like", wenn gerade einer oder mehrere Punkte gelesen worden sind.

dot_like([dot| _]).
dot_like([cdr_| _]).


%%% string_like(+State)

string_like([string, _, _]).
string_like([symbol, _, _]).



	
%%%% Zeichenklassen



impossible_dispatch_macro_character(41). % )
impossible_dispatch_macro_character(60). % <
impossible_dispatch_macro_character(C) :- C<33.
impossible_dispatch_macro_character(C) :- C>255.


%%% symbol_terminator(-Char)
% trifft auf Zeichen zu, die ein Symbol immer abschlie�en (ohne Teil von ihm zu sein)

symbol_terminator(34). % "
symbol_terminator(39). % '
symbol_terminator(40). % (
symbol_terminator(41). % )
symbol_terminator(44). % ,
symbol_terminator(59). % ;
symbol_terminator(96). % `
symbol_terminator(124). % |
symbol_terminator(X) :- X<33.
symbol_terminator(X) :- X>255.


%%% symbol_char(-Char, +Char)
% nimmt ein Zeichen oder eine Liste der Form [C] und gibt das Zeichen zur�ck, wenn es
% als Teil eines Symbols eingelesen werden kann. Die Listenform dient zur Codierung von
% (durch '\') gesch�tzten Zeichen. Da gesch�tzte Zeichen immer als Teil eines Symbols
% auftreten k�nnen, gibt symbol_char in diesem Fall einfach das Zeichen in seiner normalen
% Form zur�ck.

symbol_char([C], C) :- !.
symbol_char(C, _) :- escape_char(C), !, fail.
symbol_char(C, C) :- \+ symbol_terminator(C).


symbol_start(C, C1) :- \+ hash_sign(C), symbol_char(C, C1).


end_of_file(-1).
end_of_file(26).


%%% tag_char(-C)
% Wahr f�r solche Zeichen, die sich f�r eine Aufgabenmarkierung innerhalb eines 
% Lisp-Programms eignen.

tag_char(C) :- C>32, C<256.


escape_char(92). % \


