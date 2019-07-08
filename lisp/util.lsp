;; util.lsp
;; 181227

;; crida per teclat de les funcions
(defun c:FAC ()	(command "_UNDO" "_BEGIN") 
	(main-fac) (command "_UNDO" "_END") (terpri))


;; funcio principal que dibuixa una filera horitzontal 
;; de linies, separades entre elles segons la mida dels 
;; diametres dels cercles seleccionats
(defun main-fac (/ obj-grp obj-index obj-list diameter diam-list)
	(prompt "Select circles: ")
	(setq obj-grp (ssget))
	(setq obj-index 0)
	(repeat (sslength obj-grp)
		(setq obj-name (ssname obj-grp obj-index))
		(setq obj-list (entget obj-name))
		(if (= (cdr (assoc 0 obj-list)) "CIRCLE")
			(progn
				(setq diameter (* (cdr (assoc 40 obj-list)) 2))
				(setq diameter (round-num diameter 2))
				(setq diam-list (append (list diameter) diam-list))
			)
		)
		(setq obj-index (+ obj-index 1))
	)

	(foreach diam diam-list (print (rtos diam)))
	(draw-diameter-lines diam-list)
)


;; dibuixa una filera horitzontal de linies, separades
;; entre elles segons la llista de distàncies donada
(defun draw-diameter-lines (diam-list / start-point line-length 
		end-point index)
	(setq start-point (getpoint "Click lines start point: "))
	(if (not start-point) (setq start-point (getvar "viewctr")))
	(setq start-point (append 
		(list (round-num (car start-point) 2))
		(list (round-num (cadr start-point) 2)) '(0))
	)
	(setq line-length (car diam-list))
	(setq index 0)
	(repeat (+ (length diam-list) 1)
		(setq end-point (polar start-point (/ pi 2) line-length))
		(command "_LINE" start-point end-point "")
		(if (< index (length diam-list))
			(setq start-point 
				(polar start-point 0 (nth index diam-list)))
		)
		(setq index (+ index 1))
	)
)


;;;TODO: debug
;;;gira un o més objectes designant entitat de referència amb la qual s'alinea
(defun c:gr (/ os ent1 len1 conj pd1 px1 ang1 py1 ent2 len2 pd2	px2 ang2 py2 p10 p01 p20 p02)
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	(command "_ucs" "_s" "actual" "_y" ^c^c)
	(command "_ucs" "_w")
	(prompt "\nDesigna entitat de referència per girar: ")
	(setq ent1 (entsel))
	(setq len1 (entget (car ent1)))
	(prompt "\nSel.lecciona altres objectes per girar: ")
	(setq conj (ssget))
	(if (= conj nil)
	(setq conj (ssadd))
	)
	(setq conj (ssadd (car ent1) conj))
	(if (= (cdr (assoc 0 len1)) "MTEXT")
	(progn
		(setq pd1 (cdr (assoc 10 len1)))
		(setq px1 (cdr (assoc 11 len1)))
		(setq ang1 (angle '(0 0 0) px1))
		(setq py1 (polar '(0 0 0) (+ ang1 (/ pi 2)) 1))
		(command "_ucs" "_n" "_3" '(0 0 0) px1 py1)
	)
	(progn
		(setq pd1 (car (cdr ent1)))
		(command "_ucs" "_n" "_ob" pd1)
	)
	)
	(setq p10 (trans pd1 0 1))
	(setq p01 (trans (polar p10 0 1) 1 0))
	(command "_ucs" "_w")
	(prompt "\nDesigna entitat amb alineació de referència: ")
	(setq ent2 (entsel))
	(setq len2 (entget (car ent2)))
	(if (= (cdr (assoc 0 len2)) "MTEXT")
	(progn
		(setq pd2 (cdr (assoc 10 len2)))
		(setq px2 (cdr (assoc 11 len2)))
		(setq ang2 (angle '(0 0 0) px2))
		(setq py2 (polar '(0 0 0) (+ ang2 (/ pi 2)) 1))
		(command "_ucs" "_n" "_3" '(0 0 0) px2 py2)
	)
	(progn
		(setq pd2 (car (cdr ent2)))
		(command "_ucs" "_n" "_ob" pd2)
	)
	)
	(setq p20 (trans pd1 0 1))
	(setq p02 (trans (polar p20 0 1) 1 0))
	(command "_ucs" "_w")
	(command "_rotate" conj "" pd1 "_r" pd1 p01 p02)
	(command "_ucs" "_r" "actual")
	(setvar "osmode" os)
	(setvar "cmdecho" 1)
)


(defun c:ou (/ os p0fw p1fw ent len pb conj p0iw p1iw angi angf gir)
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	(command "_ucs" "_s" "actual" "_y" ^c^c)
	(setq p0fw (trans '(0 0 0) 1 0))
	(setq p1fw (trans '(1 0 0) 1 0))
	(prompt "\nDesigna entitat de referència per girar: ")
	(setq ent (entsel))
	(setq pb (cadr ent))
	(setq pb (trans pb 1 0))
	(prompt "\nSel.lecciona més objectes per girar: ")
	(setq conj (ssget))
	(if (= conj nil)(setq conj (ssadd)))
	(setq conj (ssadd (car ent) conj))
	(command "_ucs" "")
	(command "_ucs" "_ob" pb)
	(setq p0iw (trans '(0 0 0) 1 0))
	(setq p1iw (trans '(1 0 0) 1 0))
	(command "_ucs" "_w")
	(setq angi (angle p0iw p1iw))
	(setq angf (angle p0fw p1fw))
	(if (< angi angf) (setq gir (- angf angi)))	
	(if (> angi angf) (setq gir (* (- angi angf) -1)))
	(if (= angi angf) (setq gir 0))
	(setq gir (/ (* gir 180.0) pi))
	(command "_rotate" conj "" pb gir)
	(command "_ucs" "_r" "actual")
	(setvar "osmode" os)
	(setvar "cmdecho" 1)
)


;;; straight from acad 3d.lsp file
(defun C:XXP (/ pt1 pt2 ang pt3 os)
	(command "_ucsfollow" "0")
	(command "_ucs" "_w")
	(setq pt1 (getpoint "Entreu primer punt: "))
	(terpri)
	(setq pt2 (getpoint pt1 "Entreu segon punt: "))
	(terpri)
	(setq ang (angle pt1 pt2))
	(setq pt3 (polar pt1 (+ ang (/ pi 2)) 1))
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(command "_ucsfollow" "1")
	(command "_ucs" "3" pt1 pt2 pt3)
	(command "_ucsfollow" "0")
	(setvar "osmode" os)
)
;;;
(defun C:XX (/ pt1 pt2 ang pt3 os)
	(command "_ucsfollow" "0")
	(command "_ucs" "_w")
	(setq pt1 (getpoint "Entreu primer punt: "))
	(terpri)
	(setq pt2 (getpoint pt1 "Entreu segon punt: "))
	(terpri)
	(setq ang (angle pt1 pt2))
	(setq pt3 (polar pt1 (+ ang (/ pi 2)) 1))
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(command "_ucs" "3" pt1 pt2 pt3)
	(setvar "osmode" os)
)


(defun C:MM (/ obj pt1 pt2 dx dy go_on_flag pc os)
	(command "_UNDO" "_BEGIN")
	(setq go_on_flag T)
	(setq obj (ssget))
	(setq pt1 (cadr (grread T)))
	(terpri)
	(setq pt2 (getpoint pt1 "Entreu segon punt: "))
	(if (= pt2 nil) 
		(setq pt2 (cadr(grread T)))
	)
	(terpri)
	(setq dx (- (car pt2) (car pt1)))
	(setq dy (- (cadr pt2) (cadr pt1)))
	(if (= dx 0) (setq dx 0.1))
	(if (= dy 0) (setq dx 0.1))
	(if (< (/ (abs dy) (abs dx)) 0.25)
		(progn
			(setq dy 0)
			(setq dx (* (/ dx (abs dx)) *mob-dist*))
			(setq go_on_flag nil)
		)
	)
	(if (and go_on_flag (< (/ (abs dx) (abs dy)) 0.25))
		(progn
			(setq dx 0)
			(setq dy (* (/ dy (abs dy)) *mob-dist*))
			(setq go_on_flag nil)
		)
	)
	(if go_on_flag
		(progn
			(setq dx (* (/ dx (abs dx)) *mob-dist*))
			(setq dy (* (/ dy (abs dy)) *mob-dist*))
		)
	)
	(setq pc (list dx dy 0))
	(setq os (getvar "osmode"))
	(setvar "OSMODE" 0)
	(command "_MOVE" obj "" "0,0,0" pc)
	(setvar "OSMODE"os) 
	(command "_UNDO" "_END")
)





;;; més orde
(defun c:ORD (/ pnt1 pnt2 cnj1 num n nent lent lay cnj2)
  (setvar "regenmode" 0)
  (terpri)
  (setq pnt1 (getpoint "entreu primer punt: "))
  (terpri)
  (if pnt1
    (progn
      (setq pnt2 (getcorner pnt1 "entreu segon punt: "))
      (terpri)
      (prompt
	"sel.leccioneu objectes per ordre invers de visualització, de - a +: "
      )
      (terpri)
      (setq cnj1 (ssget))
      (setq num (sslength cnj1))
      (setq n 0)
      (repeat num
	(setq nent (ssname cnj1 n))
	(setq lent (entget nent))
	(setq lay (cdr (assoc 8 lent)))
	(setq cnj2 (ssget "_c" pnt1 pnt2 (list (cons 8 lay))))
	(if cnj2 (command "_draworder" cnj2 "" "F"))
	(setq n (+ n 1))
      )
    )
  )
  (setvar "regenmode" 1)
  (command "_regen")
)







;
(prompt "util.lsp successfully loaded!\n")
