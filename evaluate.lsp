
(defmacro with-gensyms (intern vars &body body)

 "Makro
  with-gensyms intern var-list &body body

  Dient zum Abk�rzen von let-K�pfen, in denen nur gensyms erzeugt und den
  Elementen von var-list zugewiesen werden. Ist intern nicht nil, werden die
  gensyms interniert."

 `(let ,(mapcar #'(lambda (s)
     `(,s (if ,intern (intern (symbol-name (gensym ,(symbol-name s))))
           (gensym ,(symbol-name s))
    ) )   )
    vars
   ) ,@body
) )


(defun reset-package (&optional (package *package*))

 "Funktion
  reset-package &optional (package *package*)

  L�scht alle internalen Symbole aus package."

 (do-symbols (sym *package*)
  (when (eql (nth-value 1 (find-symbol (symbol-name sym) *package*)) :internal)
   (unintern sym)
)))


(let ((error-info
   (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))

 (defmacro evaluate (userfile-name exmfile-name tracefile-name
   opsfile-name function equivalent parm-lists exm-pops pops &optional (repeat 0))

 "Makro
  evaluate userfile-name exmfile-name tracefile-name
     opsfile-name function equivalent parm-lists exm-pops pops &optional (repeat 0)

  Vergleicht Performanz und R�ckgabewerte zweier gleichnamiger Funktionen, die
  in einem 'Sch�lerprogramm' (userfile-name) bzw. einer 'Musterl�sungsdatei'
  (exmfile-name) definiert sind. Das Funktionssymbol ist function, das Symbol
  der �quivalenzfunktion equivalent. Diese Funktion kann auch erst in der
  Musterl�sungsdatei definiert sein, sollte in dem Fall aber auf keine anderen
  in dieser Datei definierten Funktionen oder globalen Variablen zugreifen,
  sofern diese nicht einem Package angeh�ren, das nach dem Laden der
  Musterl�sung nicht mehr aktuell ist. Der Grund daf�r ist, da� vor dem Laden
  des Sch�lerprogramms, um unerw�nschte Wiederverwendung von Musterl�sungsfunk-
  tionen zu verhindern, alle zuvor benutzten Symbole aus dem dann aktuellen
  Package entfernt werden. Dies hat auch Konsequenzen auf die Auswahl von
  �quivalenzfunktionen, falls Aufgaben verlangen, in den Resultatwerten
  internale Symbole zur�ckzugeben, die nicht schon in den Parameterlisten ent-
  halten sind. Symbole gleichen Namens, von denen das eine nicht mehr im
  Package vorhanden ist, sind n�mlich nicht einmal equalp.
  Angewendet wird function auf die Parameterlisten, zu denen sich die Elemente
  von parm-lists auswerten, nachdem die Musterl�sungsdatei geladen worden ist.
  Es ist also m�glich, die Parameter bei jedem Aufruf von evaluate automatisch
  neu zu erzeugen, etwa um eine zuf�llige Variation zu erhalten. F�r diesen Fall
  gibt das letzte Argument, repeat, an, wie oft der Test der Funktion auf allen
  Parameterlisten wiederholt werden soll (bei jeder Wiederholung werden die
  Parameterlisten neu erzeugt).
  Alle auftretenden Fehler werden in errorfile gespeichert. In Tracefile
  werden die von der eingebauten time-Funktion in bezug auf die beiden
  Versionen von function erzeugten Performanzangaben gespeichert sowie
  Funktionsaufrufe, auf denen die beiden Versionen nach Ma�gabe von equivalent
  unterschiedliche R�ckgabewerte liefern."

  (with-gensyms nil (tracefile opsfile equiv parmlists
    values-list1 values-list2 equiv-name counter-example-count exm-pop-names pop-names)

   (flet ((load-file (filename type)
           `(handler-case (load ,filename :verbose nil)
            (condition (condition)
             (format ,error-info "loading~A" ,type)
             (error condition))))

          (direct-trace-output ()
          `(setf *trace-output*
            (if (and ,tracefile (open-stream-p ,tracefile))
             ,tracefile
             (setf ,tracefile
              (open ,tracefile-name :direction :output :if-does-not-exist :create)))))

          (expr (type)
          ;; erzeugt Code f�r die Anwendung der Funktionen auf die Testparameter
          `(mapcar #'(lambda (parms)
             (handler-case
              (let ((fun (find-symbol ,(symbol-name function))))
               (cond ((null fun)
                      (error "Function ~A is undefined." ,(symbol-name function)))
                     (T (multiple-value-list
                       (eval (cons fun parms)) ;; apply funktioniert nicht f�r Makros, daher eval.
              ))))
              (condition (condition)
               (setf *print-circle* t)
               (format ,error-info "calling ~A~&~S" ,type parms)
               (error condition))))
            ,parmlists))

          (store-ops (pop-names)
           `(do-symbols (var *package* (terpri ,opsfile))
             (when (and
               (fboundp var)
               (find (symbol-name var) ,pop-names))
              (format ,opsfile "~S " var)))))

    `(let (,tracefile ,opsfile)
     (unwind-protect
      (let (,equiv ,parmlists ,values-list1 ,values-list2
        (,counter-example-count 0)
        (,equiv-name ,(symbol-name equivalent))
        (,exm-pop-names ',(mapcar #'symbol-name exm-pops))
        (,pop-names ',(mapcar #'symbol-name pops)))

       (setf ,opsfile (open ,opsfile-name :direction :output :if-does-not-exist :create))

       ;; Musterl�sungsdatei laden (vorher alle internalen Symbole l�schen)
       (funcall ',(symbol-function 'reset-package))
       ,(load-file exmfile-name "exemplary")

       ;; Parameterlisten erzeugen
       (handler-case (setf ,parmlists (list
         ,.(do ((result parm-lists (append parm-lists result)) (n repeat (decf n)))
          ((<= n 0) result))))
        (condition (condition)
         (format ,error-info "creating parameter lists")
         (error condition)))

       ;; �quivalenzfunktion zwischenspeichern
       (handler-case
        (setf ,equiv (symbol-function (find-symbol ,equiv-name)))
        (condition (condition)
         (format ,error-info "accessing equivalence function")
         (error condition)))

       ;; Registrieren von Performanz, R�ckgabewerten und Operatoren der Musterl�sung
       ,(direct-trace-output)
       (gc)
       (setf ,values-list1 (time ,(expr "exemplary")))
       ,(store-ops exm-pop-names)

        ;; Laden des Sch�lerprogramms (vorher alle internalen Symbole l�schen)
       (funcall ',(symbol-function 'reset-package))
       ,(load-file userfile-name "user")

        ;; Registrieren von Performanz, R�ckgabewerten und Operatoren der Sch�lerl�sung
       (terpri ,tracefile)
       ,(direct-trace-output) ; falls das Sch�lerprogramm den Trace-Output umgelenkt oder
                              ; geschlossen haben sollte
       (gc)
       (setf ,values-list2 (time ,(expr "user")))
       ,(store-ops pop-names)

       ;; Registrieren derjenigen Parameterlisten, auf denen Sch�ler- und Musterl�sung divergieren,
       ;;sowie deren Anteils an der Gesamtheit der Testaufrufe
       (terpri ,tracefile) (terpri ,tracefile)
       (princ "Counter-examples:" ,tracefile)
       (mapc #'(lambda (a b parms)
         ;;(print `(,a ,b))
         (handler-case
          (when (not (funcall ,equiv a b))
           (incf ,counter-example-count)
           (terpri ,tracefile)
           (cond ((null parms) (princ "()" ,tracefile))
                  (T (write parms :stream ,tracefile :case :upcase :pretty t :circle t))))
          (condition (condition)
 	   (setf *print-circle* t)
           (format ,error-info "comparing~%~S~%and~%~S" a b)
           (error condition))))
        ,values-list1 ,values-list2 ,parmlists)
       (format ,tracefile "~% ~%Correctness: ~,3F" ; (Das Leerzeichen nach dem ersten ~%
       									; dient als Endmarkierung)
        (- 1 (/ ,counter-example-count ,(* (1+ repeat) (length parm-lists)))))
      )
      ;; Aufr�umen
      (setf *trace-output* *terminal-io*)
      (close ,tracefile)
      (close ,opsfile)
 )))))

 (let ((errorfile *errorfile*))
  (unwind-protect
   (handler-case
    (load *evalfile-name*)
    (condition (condition)
     (if (= 0 (length error-info))
      (princ "directives" errorfile)
      (princ error-info errorfile))
     (setf *error-output* errorfile)
     (error condition)))
   (setf *error-output* *terminal-io*)
   (close errorfile)
 ))
)

