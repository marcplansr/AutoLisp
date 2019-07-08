; carp.lsp
; 161112
; TODO: debug and simplify code

; calculates all possible angles
(defun calc-angles ()
  (if (>= (/ pi 4) ang 0)
    (progn (setq al 0) (setq ag (/ pi 2))))
  (if (> (/ pi 2) ang (/ pi 4)) 
    (progn (setq al (/ pi 2)) (setq ag 0)))
  (if (> (* 3 (/ pi 4)) ang (/ pi 2))
    (progn (setq al (/ pi 2)) (setq ag pi)))
  (if (>= pi ang (* 3 (/ pi 4)))
    (progn (setq al pi) (setq ag (/ pi 2))))
  (if (>= (* 5 (/ pi 4)) ang pi)
    (progn (setq al pi) (setq ag (* 3 (/ pi 2)))))
  (if (> (* 3 (/ pi 2)) ang (* 5 (/ pi 4)))
    (progn (setq al (* 3 (/ pi 2)))(setq ag pi)))
  (if (> (* 7 (/ pi 4)) ang (* 3 (/ pi 2)))
    (progn (setq al (* 3 (/ pi 2)))(setq ag 0)))
  (if (>= (* 2 pi) ang (* 7 (/ pi 4)))
    (progn (setq al 0) (setq ag (* 3 (/ pi 2)))))
)


; points input and length and width calc
(defun enter-points ()
	(if (= p01 nil)
		(setq p01 (getpoint "Pick first point: ")) (terpri))
	(setq p04 (getpoint "Pick second point: ")) (terpri)
	(setq ang (angle p01 p04))
	(setq dis (distance p01 p04))
	(setq dg (abs (* dis (sin ang))))
	(setq dl (abs (* dis (cos ang))))
	(if (> dg dl)
		(progn
			(setq dg (abs (* dis (cos ang))))
			(setq dl (abs (* dis (sin ang))))
		)
	)
	(setq dp (- dl (* 2 *frame*)))
)


; calculates door frame points
(defun calc-frame-points ()
	(setq p02 (polar p01 al dl))
	(setq p03 (polar p01 ag dg))
	(setq p05 (polar p01 al *frame*))
	(setq p07 (polar p03 al *frame*))
	(setq p06 (polar p05 al dp))
	(setq p08 (polar p07 al dp))
)


; calculates door leaf points
(defun calc-leaf-points (clear)
	(setq p09 (polar p05 al *leaf*))
	(setq p10 (polar p09 (+ ag pi) clear))
	(setq p11 (polar p05 (+ ag pi) clear))
)


; calculates door glass points
(defun calc-glass-points ()
	(setq p09a (polar p09 (+ ag pi) *frame*))
	(setq p09b (polar p09a (+ al pi) (/ *leaf* 2)))
	(setq p09c (polar p09b ag 0.025))
	(setq p09d (polar p09a (+ al pi) *leaf*))
	(setq p10a (polar p10 ag *frame*))
	(setq p10b (polar p10a (+ al pi) (/ *leaf* 2)))
	(setq p10c (polar p10b (+ ag pi) 0.025))
	(setq p10d (polar p10a (+ al pi) *leaf*))
)


; calculates double door points
(defun calc-double-points (clear)
	(setq p12 (polar p06 (+ al pi) *leaf*))
	(setq p13 (polar p12 (+ ag pi) (- dp clear)))
	(setq p14 (polar p06 (+ ag pi) (- dp clear)))
	(setq p15 (polar p06 (+ al pi) (- dp clear)))
	(setq p16 (polar p15 (+ ag pi) 1))
)


; calculates sliding door leaf points
(defun calc-sliding-points ()
  (setq p09 (polar p05 (+ ag pi) (/ *leaf* 2)))
  (setq p10 (polar p09 al (/ dp 3)))
  (setq p11 (polar p10 (+ al pi) dp))
  (setq p12 (polar p10 (+ ag pi) *leaf*))
  (setq p13 (polar p11 (+ ag pi) *leaf*))
)


; changes new entities to their layers
(defun change-layer ()
	(setvar "cmddia" 1)
	(command "_-LAYER" "_N" "z" "_C" "3" "z" "")
	(command "_CHANGE" grp1  "" "_P" "_LA" "z" "")
	(command "_-LAYER" "_N" "fust" "_C" "34" "fust" "")
	(command "_CHANGE" grp2  "" "_P" "_LA" "fust" "")
	(setvar "cmddia" 0)
)


; draws symmetric double door 
(defun sym-double-door (p01 /)
	(enter-points)
	(calc-angles)
	(calc-frame-points)
	(calc-leaf-points (/ dp 2))
	(calc-double-points (/ dp 2))
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "orthomode" 0)
	(command "_PLINE" p01 p02 p04 p03 "C")
	(setq grp1 (ssadd (entlast)))
	(command "_PLINE" p10 p09 p05 p01 p03 p07 p05 p11 p10 "")
	(setq grp2 (ssadd (entlast)))
	(command "_ARC" p15 "_E" p10 "_D" p16)
	(ssadd (entlast) grp2)
	(command "_arc" p15 "_e" p13 "_d" p16)
	(ssadd (entlast) grp2)
	(command "_pline" p13 p12 p06 p02 p04 p08 p06 p14 p13 "")
	(ssadd (entlast) grp2)
	(command "_PEDIT" "_LAST" "_J" grp2 "" "")
	(change-layer)
	(setvar "osmode" os) 
	(command "_UNDO" "_END")
	(princ)
)


; draws asymmetric double door 
(defun asym-double-door ()
	(prompt "Door main leaf size = *clearance*") (terpri)
	(enter-points)
	(calc-angles)
	(calc-frame-points)
	(calc-leaf-points *clearance*)
	(calc-double-points *clearance*)
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "orthomode" 0)
	(command "_PLINE" p01 p02 p04 p03 "C")
	(setq grp1 (ssadd (entlast)))
	(command "_PLINE" p10 p09 p05 p01 p03 p07 p05 p11 p10 "")
	(setq grp2 (ssadd (entlast)))
	(command "_ARC" p15 "_E" p10 "_D" p16)
	(ssadd (entlast) grp2)
	(command "_arc" p15 "_e" p13 "_d" p16)
	(ssadd (entlast) grp2)
	(command "_pline" p13 p12 p06 p02 p04 p08 p06 p14 p13 "")
	(ssadd (entlast) grp2)
	(command "_PEDIT" "_LAST" "_J" grp2 "" "")
	(change-layer)
	(setvar "osmode" os) 
	(command "_UNDO" "_END")
	(princ)
)


; draws double door, symmetric and asymmetric optional
(defun c:PK ( / p01 p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 
		p12 p13 p14 p15 p16 ang dis dg dl al ag dp os grp1 grp2)
	(command "_UNDO" "_BEGIN")
	(setq p01 (getpoint "Pick first point <Enter for asymmetric>: ")) (terpri)
	(if (= p01 nil) (asym-double-door) (sym-double-door p01))
	(command "_UNDO" "_END")
)


; draws simple door
(defun c:PO ( / p01 p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 
		ang dis dg dl al ag dp os grp1 grp2)
	(command "_UNDO" "_BEGIN")
	(enter-points)
	(calc-angles)
	(calc-frame-points)
	(calc-leaf-points dp)
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "orthomode" 0)
	(command "_PLINE" p01 p02 p04 p03 "C")
	(setq grp1 (ssadd (entlast)))
	(command "_PLINE" p06 p02 p04 p08 p06 "_ARC" p10 "L" 
		p11 p05 p07 p03 p01 p05 p09 p10 "")
	(setq grp2 (ssadd (entlast)))
	(change-layer)
	(setvar "osmode" os) 
	(command "_UNDO" "_END")
	(princ)
)


; draws simple frame with no leaf
(defun c:PZ ( / p01 p02 p03 p04 p05 p06 p07 p08
		ang dis dg dl al ag dp os grp1 grp2)
	(command "_UNDO" "_BEGIN")
	(enter-points)
	(calc-angles)
	(calc-frame-points)
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "orthomode" 0)
	(command "_PLINE" p01 p02 p04 p03 "C")
	(setq grp1 (ssadd (entlast)))
	(command "_PLINE" p01 p05 p07 p03 "C")
	(setq grp2 (ssadd (entlast)))
	(command "_PLINE" p04 p08 p06 p02 "C")
	(ssadd (entlast) grp2)
	(change-layer)
	(setvar "osmode" os) 
	(command "_UNDO" "_END")
	(princ)
)


; draws simple glass door
(defun c:PV ( / p01 p02 p03 p04 p05 p06 p07 p08 p09 p10 p11
		p09a p09b p09c p09d p10a p10b p10c p10d 
		ang dis dg dl al ag dp os grp1 grp2)
	(command "_UNDO" "_BEGIN")
	(enter-points)
	(calc-angles)
	(calc-frame-points)
	(calc-leaf-points dp)PC
	(calc-glass-points)
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "orthomode" 0)
	(command "_PLINE" p01 p02 p04 p03 "C")
	(setq grp1 (ssadd (entlast)))
	(command "_PLINE" p06 p02 p04 p08 p06 "_ARC" p10 "L" 
			p11 p05 p07 p03 p01 p05 p09 p10 
			p10a p10d p10b p10c p09c p09b p09d p09a "")
	(setq grp2 (ssadd (entlast)))
	(change-layer)
	(setvar "osmode" os) 
	(command "_UNDO" "_END")
	(princ)
)


; draws sliding door
(defun c:PC ( / p01 p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 p12 p13 
		ang dis dg dl al ag grp1 grp2)
	(command "_UNDO" "_BEGIN")
	(enter-points)
	(calc-angles)
	(calc-frame-points)
	(calc-sliding-points)
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "orthomode" 0)
	(command "_PLINE" p01 p02 p04 p03 "C")
	(setq grp1 (ssadd (entlast)))
	(command "_PLINE" p01 p03 p07 p05 p01 "")
	(setq grp2 (ssadd (entlast)))
	(command "_PLINE" p02 p04 p08 p06 p02 "")
	(ssadd (entlast) grp2)
	(command "_PLINE" p10 p12 p13 p11 p10 "")
	(ssadd (entlast) grp2)
	(change-layer)
	(setvar "osmode" os) 
	(command "_UNDO" "_END")
	(princ)
)

;;; quarantine

;;;funció auxiliar, avalua els diferents casos possibles d'angles
(defun tipus ()
  (if (>= (/ pi 4) ang 0)
    (progn (setq al 0) (setq ag (/ pi 2)))
  )
  (if (> (/ pi 2) ang (/ pi 4)) 
    (progn (setq al (/ pi 2)) (setq ag 0))
  )
  (if (> (* 3 (/ pi 4)) ang (/ pi 2))
    (progn (setq al (/ pi 2)) (setq ag pi))
  )
  (if (>= pi ang (* 3 (/ pi 4)))
    (progn (setq al pi) (setq ag (/ pi 2)))
  )
  (if (>= (* 5 (/ pi 4)) ang pi)
    (progn (setq al pi) (setq ag (* 3 (/ pi 2))))
  )
  (if (> (* 3 (/ pi 2)) ang (* 5 (/ pi 4)))
    (progn (setq al (* 3 (/ pi 2)))(setq ag pi))
  )
  (if (> (* 7 (/ pi 4)) ang (* 3 (/ pi 2)))
    (progn (setq al (* 3 (/ pi 2)))(setq ag 0))
  )
  (if (>= (* 2 pi) ang (* 7 (/ pi 4)))
    (progn (setq al 0) (setq ag (* 3 (/ pi 2))))
  )
)
;;;
;;;

;;; finestra corredera dos fulles
(defun c:fc ( / p01 p00 p000 ang dis dg dl al ag dp p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 p12
	     p09a p09b p09c p09d p10a p10b p10c p10d p11a p11b p11c p11d p12a p12b p12c p12d os)
  (command "_undo" "_begin")
  (setq p01 (getpoint "Entreu primera cantonada, origen finestra: ")) (terpri)
  (setq p00 (getpoint "Entreu cantonada oposada: ")) (terpri)
  (setq ang (angle p01 p00))
  (setq dis (distance p01 p00))
  (setq dg (abs (* dis (sin ang))))
  (setq dl (abs (* dis (cos ang))))
  (if (> dg dl)(progn
      (setq dg (abs (* dis (cos ang))))
      (setq dl (abs (* dis (sin ang))))
  ))
  (tipus)
  ;;; trobem la resta de punts
  (setq p02 (polar p01 al dl))
  (setq p04 (polar p02 ag 0.10))
  (setq p03 (polar p01 ag 0.10))
  (setq p05 (polar p01 al 0.05))
  (setq p07 (polar p03 al 0.05))
  (setq p06 (polar p02 (+ al pi) 0.05))
  (setq p08 (polar p04 (+ al pi) 0.05))
  (setq p09 (polar p05 ag 0.05))
  (setq p10 (polar p06 ag 0.05))
  (setq p11 (polar p01 al (+ (/ dl 2) 0.025)))
  (setq p12 (polar p04 (+ al pi) (+ (/ dl 2) 0.025)))
  (setq p09a (polar p09 al 0.05))
  (setq p09b (polar p09a (+ ag pi) 0.025))
  (setq p09c (polar p09a (+ ag pi) 0.05))
  (setq p09d (polar p09b (+ al pi) 0.025))
  (setq p10a (polar p10 (+ al pi) 0.05))
  (setq p10b (polar p10a ag 0.025))
  (setq p10c (polar p10a ag 0.05))
  (setq p10d (polar p10b al 0.025))
  (setq p11a (polar p11 (+ al pi) 0.05))
  (setq p11b (polar p11a ag 0.025))
  (setq p11c (polar p11a ag 0.05))
  (setq p11d (polar p11b al 0.025))
  (setq p12a (polar p12 al 0.05))
  (setq p12b (polar p12a (+ ag pi) 0.025))
  (setq p12c (polar p12a (+ ag pi) 0.05))
  (setq p12d (polar p12b (+ al pi) 0.025))
  (setq p000 (polar p01 ag dg))
  (setq os (getvar "osmode")) (setvar "osmode" 0) (setvar "orthomode" 0)
  (command "_pline" p01 p02 p04 p03 p01 p05 p07 p09 p10 p06 p08 p10c p10a p10b p10d
	   p12d p12b p12a p12c p11 p11a p12 p11c p11b p11d p09d p09b p09c p09a "")
  (setvar "cmddia" 1)
  (command "_-layer" "_n" "fust" "_c" "34" "fust" "")
  (command "_change" "_last"  "" "_p" "_la" "fust" "")
  (command "_-layer" "_n" "prj" "_c" "252" "prj" "")
  (command "_line" p00 p000 "")
  (command "_change" "_last"  "" "_p" "_la" "prj" "")
  (command "_-layer" "_n" "z" "_c" "3" "z" "")
  (command "_line" p01 p02 "")
  (command "_change" "_last"  "" "_p" "_la" "z" "")
  (setvar "osmode" os) (setvar "cmddia" 0)
  (command "_undo" "_end")
)
;;;
;;;
;;;
;;;
;;; finestra batent dos fulles
(defun c:fb ( / p01 p00 p000 ang dis dg dl al ag dp p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 p12 p13 p14
	     p09a p09b p09c p09d p10a p10b p10c p10d p11a p11b p11c p11d p11e p11f p11g p11h p11i conj os)
  (command "_undo" "_begin")
  (setq p01 (getpoint "Entreu primera cantonada, origen finestra: ")) (terpri)
  (setq p00 (getpoint "Entreu cantonada oposada: ")) (terpri)
  (setq ang (angle p01 p00))
  (setq dis (distance p01 p00))
  (setq dg (abs (* dis (sin ang))))
  (setq dl (abs (* dis (cos ang))))
  (if (> dg dl)(progn
      (setq dg (abs (* dis (cos ang))))
      (setq dl (abs (* dis (sin ang))))
  ))
  (tipus)
  ;;; trobem la resta de punts
  (setq p02 (polar p01 al dl))
  (setq p04 (polar p02 ag 0.10))
  (setq p03 (polar p01 ag 0.10))
  (setq p05 (polar p01 al 0.05))
  (setq p07 (polar p03 al 0.05))
  (setq p06 (polar p02 (+ al pi) 0.05))
  (setq p08 (polar p04 (+ al pi) 0.05))
  (setq p09 (polar p05 ag 0.05))
  (setq p10 (polar p06 ag 0.05))
  (setq p11 (polar p01 al (/ dl 2)))
  (setq p09a (polar p09 al 0.05))
  (setq p09b (polar p09a (+ ag pi) 0.025))
  (setq p09c (polar p09a (+ ag pi) 0.05))
  (setq p09d (polar p09b (+ al pi) 0.025))
  (setq p10a (polar p10 (+ al pi) 0.05))
  (setq p10b (polar p10a (+ ag pi) 0.025))
  (setq p10c (polar p10a (+ ag pi) 0.05))
  (setq p10d (polar p10b al 0.025))
  (setq p11a (polar p11 (+ al pi) 0.05))
  (setq p11b (polar p11a ag 0.025))
  (setq p11c (polar p11b al 0.025))
  (setq p11d (polar p11b ag 0.025))
  (setq p11e (polar p11d al 0.05))
  (setq p11f (polar p11e al 0.05))
  (setq p11g (polar p11f (+ ag pi) 0.025))
  (setq p11h (polar p11g (+ al pi) 0.025))
  (setq p11i (polar p11g (+ ag pi) 0.025))
  (setq p12 (polar p05 (+ ag pi) (/ (- dl 0.10) 2)))
  (setq p13 (polar p06 (+ ag pi) (/ (- dl 0.10) 2)))
  (setq p14 (polar p11 (+ ag pi) (/ (- dl 0.10) 2)))
  (setq p000 (polar p01 ag dg))
  (setq os (getvar "osmode")) (setvar "osmode" 0) (setvar "orthomode" 0)
  (command "_pline" p03 p01 p02 p04 p03 p07 p05 p09 p10 p08 p06 p10 p10a p10c p10b p10d
	   p11h p11g p11i p11f p11e p11 p11e p11d p11a p11b p11c p09d p09b p09c p09a "")
  (setvar "cmddia" 1)
  (command "_-layer" "_n" "fust" "_c" "34" "fust" "")
  (command "_change" "_last"  "" "_p" "_la" "fust" "")
  (command "_-layer" "_n" "dot" "_c" "_cyan" "dot" "_lt" "dot" "dot" "")
  (command "_arc" p11 "_e" p12 "_d" p14)
  (setq conj (ssadd (entlast)))
  (command "_arc" p11 "_e" p13 "_d" p14)
  (ssadd (entlast) conj)
  (command "_pline" p12 p05 "")
  (ssadd (entlast) conj)
  (command "_pline" p13 p06 "")
  (ssadd (entlast) conj)
  (command "_pedit" "_last" "_j" conj "" "")
  (command "_change" "_last"  "" "_p" "_la" "dot" "")
  (command "_-layer" "_n" "prj" "_c" "252" "prj" "")
  (command "_line" p00 p000 "")
  (command "_change" "_last"  "" "_p" "_la" "prj" "")
  (command "_-layer" "_n" "z" "_c" "3" "z" "")
  (command "_line" p01 p02 "")
  (command "_change" "_last"  "" "_p" "_la" "z" "")
  (setvar "osmode" os) (setvar "cmddia" 0)
  (command "_undo" "_end")
)
;;;
;;;
;;;
;;;
;;; finestra batent una fulla
(defun c:fs ( / p01 p00 p000 ang dis dg dl al ag dp p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 p12
	     p09a p09b p09c p09d p10a p10b p10c p10d conj os)
  (command "_undo" "_begin")
  (setq p01 (getpoint "Entreu primera cantonada, origen finestra: ")) (terpri)
  (setq p00 (getpoint "Entreu cantonada oposada: ")) (terpri)
  (setq ang (angle p01 p00))
  (setq dis (distance p01 p00))
  (setq dg (abs (* dis (sin ang))))
  (setq dl (abs (* dis (cos ang))))
  (if (> dg dl)(progn
      (setq dg (abs (* dis (cos ang))))
      (setq dl (abs (* dis (sin ang))))
  ))
  (tipus)
  ;;; trobem la resta de punts
  (setq p02 (polar p01 al dl))
  (setq p04 (polar p02 ag 0.10))
  (setq p03 (polar p01 ag 0.10))
  (setq p05 (polar p01 al 0.05))
  (setq p07 (polar p03 al 0.05))
  (setq p06 (polar p02 (+ al pi) 0.05))
  (setq p08 (polar p04 (+ al pi) 0.05))
  (setq p09 (polar p05 ag 0.05))
  (setq p10 (polar p06 ag 0.05))
  (setq p09a (polar p09 al 0.05))
  (setq p09b (polar p09a (+ ag pi) 0.025))
  (setq p09c (polar p09a (+ ag pi) 0.05))
  (setq p09d (polar p09b (+ al pi) 0.025))
  (setq p10a (polar p10 (+ al pi) 0.05))
  (setq p10b (polar p10a (+ ag pi) 0.025))
  (setq p10c (polar p10a (+ ag pi) 0.05))
  (setq p10d (polar p10b al 0.025))
  (setq p11 (polar p05 (+ ag pi) (- dl 0.10)))
  (setq p12 (polar p06 (+ ag pi) (- dl 0.10)))
  (setq p000 (polar p01 ag dg))
  (setq os (getvar "osmode")) (setvar "osmode" 0) (setvar "orthomode" 0)
  (command "_pline" p03 p01 p02 p04 p03 p07 p05 p09 p10 p08 p06 p10 p10a p10c p10b p10d p09d p09b p09c p09a "")
  (setvar "cmddia" 1)
  (command "_-layer" "_n" "fust" "_c" "34" "fust" "")
  (command "_change" "_last"  "" "_p" "_la" "fust" "")
  (command "_-layer" "_n" "dot" "_c" "_cyan" "dot" "_lt" "dot" "dot" "")
  (command "_arc" p11 "_e" p06 "_d" p12)
  (setq conj (ssadd (entlast)))
  (command "_pline" p11 p05 "")
  (ssadd (entlast) conj)
  (command "_pedit" "_last" "_j" conj "" "")
  (command "_change" "_last"  "" "_p" "_la" "dot" "")
  (command "_-layer" "_n" "prj" "_c" "252" "prj" "")
  (command "_line" p00 p000 "")
  (command "_change" "_last"  "" "_p" "_la" "prj" "")
  (command "_-layer" "_n" "z" "_c" "3" "z" "")
  (command "_line" p01 p02 "")
  (command "_change" "_last"  "" "_p" "_la" "z" "")
  (setvar "osmode" os) (setvar "cmddia" 0)
  (command "_undo" "_end")
)






;
(prompt "carp.lsp successfully loaded!\n")
