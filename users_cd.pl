%%% user(+Username, ?Password, -Forename, -Userpath_as_atom, -UserHistory)
% Liefert Angaben über die Benutzer des Systems.
% Die Benutzergeschichte (UserHistory) ist mit einer leeren Liste zu initialisieren.

:- dynamic user/5.

user(jan, 111, 'Jan', 'C:/Jans Dateien/sulla/', [[1.2, 5, [], 4.99835], [1.1, 5, [], 4.64729]]).
