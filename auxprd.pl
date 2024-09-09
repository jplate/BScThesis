%%%%%% Allgemeine Hilfsprädikate:





%%%%% Operatoren




%%% Goal >+ Goals
% Wenn Goal bewiesen werden kann, ist der Beweis fertig; sonst müssen dafür 
% Goals bewiesen werden. (Um ihn mit einem "echten" Namen zu belegen, könnte
% man ihn daher vielleicht "(Conditional) Return" nennen.)
% 
% Weil die Priorität dieses Operators gleich der des Kommas ist, kann man
% ein Code-Segment der Form Goal>+ syntaktisch beinahe (d.h. bis auf das vom
% nächsten Goal trennende Komma) wie ein einfaches Goal behandeln.
% Der Vorteil der Anwendung dieses Operators: "Kürzere" und weniger tief 
% verschachtelte Klammern (falls die Alternative in der Benutzung von Semikolons 
% liegt) bzw. weniger Hilfsprädikate (nämlich eines für jedes benutzte ">+").

:- op(1000, xfy, >+).

A >+ _ :- A, !.
_ >+ B :- B.


	


%%%%% Listenprädikate




%%% delete1(?List1, ?Item, ?List2)

delete1([X| Y], X, Y).
delete1([Z| Y], X, [Z| Y1]) :-
	delete1(Y, X, Y1).


	
%%% reverse_all(+X, -Result)

reverse_all(X, X) :- var(X), !.
reverse_all([H| T], Result) :- !, 
	reverse_all_([H| T], [], Result).
reverse_all(X, X).


%%% reverse_all_(+List1, +Acc, -List2)

reverse_all_([], Acc, Acc) :- !.
reverse_all_([X| R], Acc, Result) :-
	reverse_all(X, Xr), 
	reverse_all_(R, [Xr| Acc], Result).


%%% mk_list(+N, +Object, -List)

mk_list(0, _, []).
mk_list(N, O, [O| R]) :- N>0,
	M is N-1,
	mk_list(M, O, R).
	

%%% member(+Object, +List, -List)

member(X, [X| R], R).
member(X, [_| R], S) :- member(X, R, S).


%%% first_member(+Object, +List)

first_member(X, L) :- member(X, L), !.


%%% first_member(+Object, +List, -List)

first_member(X, L, R) :- member(X, L, R), !.


%%% deep_member(+X, +List1, -List2)

deep_member(X, [X| R], R).
deep_member(X, [Y| R], [Yr| R]) :-
	deep_member(X, Y, Yr).
deep_member(X, [_| R], R1) :-
	deep_member(X, R, R1).



%%% rev_append(List1, List2, List3)

rev_append([], List2, List2) :- !.
rev_append([X| R], L, L1) :-
	rev_append(R, [X| L], L1).



%%% divide_list_at(+N, +List, -List1, -List2)

divide_list_at(N, List, List1, List2) :-
	divide_list_at_(N, List, [], List1, List2).


%%% divide_list_at_(+N, +List, +Acc, -List1, -List2)

divide_list_at_(0, Rest, Acc, List, Rest) :- !,
	reverse(Acc, List).
divide_list_at_(N, [X| R], Acc, List1, List2) :-
	N1 is N - 1,
	divide_list_at_(N1, R, [X| Acc], List1, List2).



%%% replace_members(+List1, +Replacements, -List2)
% Ersetzt Elemente in List1 gemäß Replacements durch Listensegmente und gibt das
% Resultat in List2 zurück.
% Replacements hat die Form [[Member, ListSegment], ...]

replace_members(List, Replacements, List1) :-
	replacemembers(List, Replacements, [], List1).


%%% replacemembers(+List1, +Replacements, +Acc, -List2)
% Hilfsprädikat von replace_members.

replacemembers([], _, Acc, Result) :- !, 
	reverse(Acc, Result).
replacemembers([X| R], Replacements, Acc, Result) :-
	member([X, XL], Replacements), !, 
	rev_append(XL, Acc, Acc1),
	replacemembers(R, Replacements, Acc1, Result).
replacemembers([X| R], Replacements, Acc, Result) :-
	replacemembers(R, Replacements, [X| Acc], Result).



%%% goalsort(+List, +Predicate, +List_element, +Key, +Goal, -SortedList)
% Sortiert Liste, wobei die Listenelemente anhand von Predicate,
% angewandt auf die von Goal extahierten Key-Terme, verglichen werden.
% Predicate funktioniert dabei wie das Standard-Vergleichsprädikat
% compare/3.
% Um eine Liste von Listen anhand von deren ersten Elementen 
% zu sortieren, rufe man goal_sort(List, compare, E, X, E=[X| _], Sorted)
% auf.

goalsort(List, Predicate, Element, Key, Goal, List1) :-
	gensym(gs_extract, GS_extract),
	HeadE =.. [GS_extract, Element, Key],
	assert(HeadE :- Goal),

	gensym(gs_pred, GS_pred),
	HeadP =.. [GS_pred, Delta, E1, E2],
	Extract1 =.. [GS_extract, E1, Key1],
	Extract2 =.. [GS_extract, E2, Key2],
	Compare =.. [Predicate, Delta, Key1, Key2],
	assert((HeadP :- Extract1, Extract2, Compare)),
	
	predsort(GS_pred, List, List1),

	retractall(HeadE),
	retractall(HeadP).



%%% extract_list(+Functor/Arity, +List1, +Acc1, +Acc2, -List2, -Rest)
% Liefert zu einer Liste List1 eine Liste List2 der in List1
% enthaltenen Elemente, deren Funktor Functor ist und die Stelligkeit
% Arity besitzt. In Rest werden die nicht in List2 aufgenommenen Elemente
% zurückgegeben. Acc1 ist der Akkumulator für List2, Acc2 der für
% Rest.

extract_list(_/_, [], Acc1, Acc2, List, Rest) :- !,
	reverse(Acc1, List),
	reverse(Acc2, Rest).
extract_list(F/A, [X| R], Acc1, Acc2, List, Rest) :-
	X =.. [F| L], length(L, A), !,
	extract_list(F/A, R, [X| Acc1], Acc2, List, Rest).
extract_list(F/A, [X| R], Acc1, Acc2, List, Rest) :-
	extract_list(F/A, R, Acc1, [X| Acc2], List, Rest).





%%%%% Differenzlisten




%%% nl(-DList, +Rest)

nl -->
	[NL], {newline_char(NL)}.


%%% separate(+Separator, +Rest_list, -Text, ?Rest)

separate(_, []) --> !,
	[].
separate(S, _) -->
	[S].


%%% attach_lines(+Lines, -List, ?Rest)
% Liefert zu einer Liste von Zeilenrepräsentationen eine Differenzliste,
% in der die "Wörter" der einzelnen Zeilen von Leerzeichen gefolgt werden
% und die jeweils letzten Wörter jeder Zeile zusätzlich von einem
% Zeilenumbruch.

attach_lines(Lines) -->
	{newline_char(Nl)},
	attach(Lines, [Nl, ' ']).


%%% attach(+List1, +Separator_list, -List2, ?Rest)
% Liefert eine Differenzliste, die als Elemente alle Elemente der ersten
% length(Separator_list) Ebenen von List1 enthält, jeweils getrennt durch das
% i-te Element von Separator_list, wenn das betreffende Element der
% i-ten Ebene von List1 angehört.

attach([], _) --> !,
	[].
attach([X], [_| SR]) --> !,
	attach(X, SR).
attach(X, []) --> !,
	[X].
attach([X| R], [S| SR]) -->
	attach(X, SR), [S],
	attach(R, [S| SR]).


%%% attach_first(+List1, +NList, +Separator_list, +Ellipsis, -List2, ?Rest)
% Wie attach, nur daß von jeder Teilliste von List1 auf jeder Ebene i nur
% soviele Elemente in das Ergebnis aufgenommen werden, wie das i-te Element
% von NList angibt. Ist dieses Element keine Zahl, werden alle Elemente auf-
% genommen. Werden Elemente einer Teilliste ausgeschlossen, wird nach den
% nicht ausgeschlossenen Elementen Ellipsis in das Ergebnis eingefügt.

attach_first([], _, _, _) --> [], !.
attach_first(_, [N| _], _, Ellipsis) -->
	{number(N), N < 1, !},
	[Ellipsis].
attach_first(X, [], [], _) --> !,
	[X].
attach_first([X| R], [N| NR], [S| SR], E) -->
	attach_first(X, NR, SR, E), [S],
	{number(N), !, N1 is N - 1;
	 N1 = N},
	attach_first(R, [N1| NR], [S| SR], E).





%%%%% Arithmetik




%%% round(+Expression, +Precision, -Result)

:- arithmetic_function(round/2).

round(X, P, Result) :-
	F is 10 ** P,
	Result is round(X / F) * F.



%%% max_of_list(+List, -Maximum)

max_of_list([X| R], N) :-
	maxoflist(R, X, N).

%%% maxoflist(+List, +Default, -Maximum)

maxoflist([], Default, Default).
maxoflist([X| R], Default, Maximum) :-
	X >= Default, !,
	maxoflist(R, X, Maximum).
maxoflist([_| R], Default, Maximum) :-
	maxoflist(R, Default, Maximum).



%%% min_of_list(+List, -Maximum)

min_of_list([X| R], N) :-
	minoflist(R, X, N).

%%% minoflist(+List, +Default, -Minimum)

minoflist([], Default, Default).
minoflist([X| R], Default, Minimum) :-
	X =< Default, !,
	minoflist(R, X, Minimum).
minoflist([_| R], Default, Minimum) :-
	minoflist(R, Default, Minimum).



%%% sum_list(+List, +Start, -Sum)

sum_list([], S, S) :- !.
sum_list([X| R], S, Sum) :-
	S1 is S + X,
	sum_list(R, S1, Sum).



%%% num_to_list(+Number, +Radix, +Precision, -[Magnitude| List])

num_to_list(X, _, _, [0]) :-
	0 =:= X, !.
num_to_list(N, R, P, [Mag| List]) :-
	R > 0,
	Mag is truncate(log(N) / log(R)),
	Num is N / (R ** Mag),
	num2list(Mag, P, Num, R, [], List).


%%% num2list(+Magnitude, +Precision, +Number, +Radix, +Acc, -List)

num2list(_, _, 0, _, Acc, List) :- !,
	reverse(Acc, List).
num2list(Mag, P, _, _, Acc, List) :-
	number(P),
	Mag < -P, !,
	reverse(Acc, List).
num2list(Mag, P, Num, R, Acc, List) :-
	Mag1 is Mag - 1,
	Digit is truncate(Num),
	Num1 is (Num - Digit) * R,
	num2list(Mag1, P, Num1, R, [Digit| Acc], List).





%%%%% Ein- und Ausgabe




%%% lines(-List)
% Liefert eine Liste der von line erzeugten Zeilenrepräsentationen bis zum Ende der Datei.

lines(List) :-
	lines_([], List).


%%% lines_(+Acc, -List)
% Hilfsprädikat von lines.

lines_(Acc, Result) :-
	line(L, End),
	(End = file,
	 reverse([L| Acc], Result))>+
	% sonst:
	lines_([L| Acc], Result).



%%% lines_until(+Line, -List)
% Liest solange Zeilen ein, bis eine mit Line unifizierbare Zeile gelesen wurde.
% Gibt eine Liste der gelesenen Zeilen zurück.

lines_until(Line, List) :-
	lines_until_(Line, [], List).


%%% lines_until_(+Line, +Acc, -List)
% Hilfsprädikat von lines_until.

lines_until_(Line, Acc, Result) :-
	line(L, End),
	((End = file; L = Line),
	 reverse([L| Acc], Result))>+
	% sonst:
	lines_until_(Line, [L| Acc], Result).



%%% line(-Line)
% Liest eine Zeile ein und gibt diese zurück.

line(Line) :-
	line(Line, _).


%%% line(-Line, -End)
% Liest eine Zeile ein und gibt diese zurück, zusammen mit einer Angabe
% über ihr Ende (line oder file).

line(Result, End) :-
	word(W, End0),
	((End0 = line; End0 = file),
	 End = End0,
	 (W = '', Result = []; Result = [W]))>+
	Result = [W| LT],
	line(LT, End).



%%% word(-Word, -End)

word(Wn, End) :-
	get0(C),
	word_(C, W, 0, End),
	name(Wn, W).


%%% word0(+Char, -List, +Counter, -End)

% Wenn das Ende der Datei erreicht worden ist:
word_(C, [], _, file) :-
	end_of_file(C), !.

% Wenn das Ende der Zeile erreicht worden ist:
word_(C, [], _, line) :-
	newline(C), !.

% Sammle die Zeichen im Wort in einer Liste:
word_(C, [C0 | W], N, End) :-
	in_word(C, C0), !,
	get0(C1),
	N1 is N+1,
	word_(C1, W, N1, End).

% Suche nach Wortanfang:
word_(_, W, 0, End) :-
	get0(C1),
	word_(C1, W, 0, End).

% Wortende:
word_(_, [], _, word) :- !.


%%% in_word(+Char, -Char)
% Mit dem -Char kann man dies Prädikat ggf. zu einem Transducer machen.

in_word(C, C) :- tag_char(C).
in_word(C, C) :- C>96, C<123.
in_word(C, C) :- C>64, C<91.
in_word(C, C) :- C>47, C<58.
in_word(40, 40). % öffnende Klammer
in_word(41, 41). % schließende Klammer



%%% user_confirm(+Text, +Yes_list, +No_list)

user_confirm(Text, YesL, NoL) :-
	line([W| _], End),
	member(W, YesL)>+
	\+ (member(W, NoL); End = file), % ein EOF gilt als "Nein"
	nl, writel(Text), !,
	user_confirm(Text, YesL, NoL).



%%% user_input(+Text, -Line, +Goal, -Var)
% Erwartet eine Inputzeile, die Goal erfüllt. Fordert den Benutzer solange mit
% Text auf, etwas einzugeben, bis ein EOF auftritt (in dem Fall schlägt der
% Beweis fehl) oder die eingegebene Zeile Goal erfüllt.

user_input(Text, Line, Goal) :-
	repeat, line(Line, End),
	(Goal, !;

	 End = file, !,
	 fail;

	 nl,
	 writel(Text),
	 fail).



%%% writel(List)

writel([]) :- !.
writel([X| R]) :-
	write(X),
	writel(R).



%%% write_items(+List)

write_items(List) :-
	newline_char(Nl), !,
	write_items(List, Nl).


%%% write_items(+List, +Delimiter)

write_items([], _).
write_items([X], _) :- !,
	write(X).
write_items([X| R], Delim) :-
	write(X), write(Delim),
	write_items(R, Delim).



%%% newline_char(?Nlc)

newline_char(Nlc) :-
	newline(Nl),
	name(Nlc, [Nl]).




%%%% Zeichenklassen:



visible_char(C) :-
	number(C), C>32, C<256.

newline(10).

carriage_return(13).

htab(9).

space(32).

double_quote(34).

hash_sign(35).

quote(39).

opening_parenthesis(40).

closing_parenthesis(41).

comma(44).

dot(46).

digit(C) :- between(48, 57, C).

colon(58).

semicolon(59).

at_sign(64).

backslash(92).

backquote(96).

vert_bar(124).




