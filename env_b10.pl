%%%%%% Umgebungsabhängige Parameter und Prozeduren






%%%% Pfade



%%% base_path(-Path, ?Rest)

base_path -->
	"/home/Sem-App/prolog/jplate/sulla/".


%%% exemplaries_path(-Path, ?Rest)

exemplaries_path -->
	"/home/Sem-App/prolog/jplate/sulla/muster/".


%%% tmp_path(-Path, ?Rest)

tmp_path -->
	"/home/Sem-App/prolog/jplate/sulla/tmp/".




%%%% Lisp-Implementations-Spezifika



%%% lisp_interpreter_call(+Expr, -Call, ?Rest)

lisp_interpreter_call(Expr) -->
	{double_quote(Dq)},
	"clisp -q -x ", [Dq], Expr, [Dq].


%%% extract_efficiency_figures(+Lines, -Run_time, -Space_consumption, -Rest_of_lines)
% Liefert zu der Ausgabe der time-Funktion der verwendeten Lisp-Implementation
% (im von lines gelieferten Format, also in der Form [[Word0, Word1, ...], ...])
% die darin enthaltenen Angaben über Speicher- und Zeitbedarf (in Sekunden bzw. Bytes).
% In Rest_of_lines wird außerdem der dazu nicht benötigte Rest von Lines zurückgegeben.

extract_efficiency_figures(Lines, Run_time, Space, Rest) :-
	deep_member('Run', Lines, [[_, Run_time, _], [_, Space, _]| Rest]).


