; cut.lsp
; 161112
; TODO: clean up


(defun c:tt ( / ent1 nent1 pdent1 lent1 pient1 pfent1 angent1 llent1 pment1 llpdpient1 llpdpfent1 llpdpment1
	          ent2 nent2 pdent2 lent2 pient2 pfent2 angent2 llent2 pment2 llpdpient2 llpdpfent2 llpdpment2
	          layent1 paux pproj llproj p1 p2 p3 p4 p12 p34 conj osm)
  (command "_undo" "_begin")
  (setq osm (getvar "osmode")) (setvar "osmode" 0)
;;; primera línia
  (setq ent1 (entsel "Designa primera línia: "))
  (setq nent1 (car ent1))
  (setq pdent1 (cadr ent1))
  (setq lent1 (entget nent1))
  (setq layent1 (cdr (assoc 8 lent1)))
  (setq pient1 (trans (cdr (assoc 10 lent1)) 0 1))
  (setq pfent1 (trans (cdr (assoc 11 lent1)) 0 1))
  (setq angent1 (angle pient1 pfent1))
  (setq llent1 (distance pient1 pfent1))
  (setq pment1 (polar pient1 angent1 (/ llent1 2)))
  (setq llpdpient1 (distance pdent1 pient1))
  (setq llpdpfent1 (distance pdent1 pfent1))
  (setq llpdpment1 (distance pdent1 pment1))
;;; segona línia
  (setq ent2 (entsel "Designa segona línia: "))
  (command "_change" ent2 "" "_p" "_la" layent1 "")
  (setq nent2 (car ent2))
  (setq pdent2 (cadr ent2))
  (setq lent2 (entget nent2))
  (setq pient2 (trans (cdr (assoc 10 lent2)) 0 1))
  (setq pfent2 (trans (cdr (assoc 11 lent2)) 0 1))
  (setq angent2 (angle pient2 pfent2))
  (setq llent2 (distance pient2 pfent2))
  (setq pment2 (polar pient2 angent2 (/ llent2 2)))
  (setq llpdpient2 (distance pdent2 pient2))
  (setq llpdpfent2 (distance pdent2 pfent2))
  (setq llpdpment2 (distance pdent2 pment2))
;;; primer cas, el punt de designació1 està més proxim al punt mig que a cap dels extrems de la línia 1
  (if (and (<= llpdpment1 llpdpient1) (<= llpdpment1 llpdpfent1))
    (setq p1 (polar pment1 (+ angent1 pi) (/ *wall-gap* 2)))
  )
;;; segon cas, el punt de designació1 està més proxim al punt inicial que al punt mig de la línia 1
  (if (> llpdpment1 llpdpient1)
    (progn
      ;;; subcas 2.1, el punt de designació2 està més proxim al punt mig que a cap dels extrems de la línia 2
      (if (and (<= llpdpment2 llpdpient2) (<= llpdpment2 llpdpfent2))
	(setq p1 (polar pient1 angent1 *gap-dist*))
      )
      ;;; subcas 2.2, el punt de designació2 està més proxim al punt inicial que al punt mig de la línia 2
      (if (> llpdpment2 llpdpient2)
	(progn
	  (setq paux (polar pient1 (+ angent1 (/ pi 2)) 1))
	  (setq pproj (inters pient2 pfent2 pient1 paux nil)) 
	  (setq llproj (distance pproj pient2))
	  (setq p1 (polar pient1 angent1 (- (/ llproj 2) (/ *wall-gap* 2))))
	)
      )
      ;;; subcas 2.3, el punt de designació2 està més proxim al punt final que al punt mig de la línia 2
      (if (> llpdpment2 llpdpfent2)
	(progn
	  (setq paux (polar pient1 (+ angent1 (/ pi 2)) 1))
	  (setq pproj (inters pient2 pfent2 pient1 paux nil)) 
	  (setq llproj (distance pproj pfent2))
	  (setq p1 (polar pient1 angent1 (- (/ llproj 2) (/ *wall-gap* 2))))
	)
      )
    )
  )
;;; tercer cas, el punt de designació1 està més proxim al punt final que al punt mig de la línia 1
  (if (> llpdpment1 llpdpfent1)
    (progn
      ;;; subcas 3.1, el punt de designació2 està més proxim al punt mig que a cap dels extrems de la línia 2
      (if (and (<= llpdpment2 llpdpient2) (<= llpdpment2 llpdpfent2))
	(setq p1 (polar pfent1 (+ angent1 pi) (+ *gap-dist* *wall-gap*)))
      )
      ;;; subcas 3.2, el punt de designació2 està més proxim al punt inicial que al punt mig de la línia 2
      (if (> llpdpment2 llpdpient2)
	(progn
          (setq paux (polar pfent1 (+ angent1 (/ pi 2)) 1))
          (setq pproj (inters pient2 pfent2 pfent1 paux nil))
          (setq llproj (distance pproj pient2))
          (setq p1 (polar pfent1 (+ angent1 pi) (+ (/ llproj 2) (/ *wall-gap* 2))))
	)
      )
      ;;; subcas 3.3, el punt de designació2 està més proxim al punt final que al punt mig de la línia 2
      (if (> llpdpment2 llpdpfent2)
	(progn
          (setq paux (polar pfent1 (+ angent1 (/ pi 2)) 1))
          (setq pproj (inters pient2 pfent2 pfent1 paux nil))
          (setq llproj (distance pproj pfent2))
          (setq p1 (polar pfent1 (+ angent1 pi) (+ (/ llproj 2) (/ *wall-gap* 2))))     
	)
      )
    )
  )
;;; trobem la resta de punts principals sobre la línia 1
  (setq p2 (polar p1 angent1 *wall-gap*))
  (setq p12 (polar p1 angent1 (/ *wall-gap* 2)))
;;; i les seves projeccions sobre la línia 2
  (setq paux (polar p12 (+ angent1 (/ pi 2)) 1 ))
  (setq p34 (inters pient2 pfent2 p12 paux nil)) 
  (setq paux (polar p1 (+ angent1 (/ pi 2)) 1 ))
  (setq p3 (inters pient2 pfent2 p1 paux nil))
  (setq paux (polar p2 (+ angent1 (/ pi 2)) 1 ))
  (setq p4 (inters pient2 pfent2 p2 paux nil))
;;; finalment fem les linies i tallem
  (command "_line" p1 p3 "")
  (setq conj (ssadd (entlast)))
  (command "_line" p2 p4 "")
  (ssadd (entlast) conj)  
  (command "_change" conj "" "_p" "_la" layent1 "")
  (command "_trim" conj "" "f" p12 p34 "" "")
  (setvar "osmode" osm)
  (command "_undo" "_end")
)



;
(prompt "cut.lsp successfully loaded!\n")