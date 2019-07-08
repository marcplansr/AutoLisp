;; dim.lsp
;; 181227


;; crida per teclat de les funcions
(defun c:DO ()	(command "_UNDO" "_BEGIN") 
	(ortho-dim) (command "_UNDO" "_END") (terpri))

(defun c:OD ()	(command "_UNDO" "_BEGIN") 
	(ortho-dim) (command "_UNDO" "_END") (terpri))

(defun ortho-dim (/ start-point end-point angle horizontal
		increment dim-point osnap)
	(setq start-point (getpoint "Pick dim start point: "))
	(terpri)
	(setq end-point (getpoint start-point "Pick dim end point: "))
	(terpri)
	(setq angle (fix-angle start-point end-point))
	;; (setq horizontal (or (= angle 0) (= angle pi)))
	(while end-point
		(if (or (= angle 0) (= angle pi))
			(setq increment (abs (- (car end-point) (car start-point))))
			(setq increment (abs (- (cadr end-point) (cadr start-point))))
		)
		(setq end-point (polar start-point angle increment))
		(setq dim-point (polar start-point (+ angle (/ pi 2)) *dit*))
		(setq osnap (getvar "osmode"))	(setvar "osmode" 0)
		(command "_DIMALIGNED" start-point end-point dim-point)
		(setvar "osmode" osnap)
		(setq start-point end-point)
		(setq end-point (getpoint start-point "Pick new dim end point: "))


	)





	;; (cond
	;; 	((= angle 0) 
	;; 		(setq increment (- (car end-point) (car start-point))))
	;; 	((= angle pi) 
	;; 		(setq increment (- (car start-point) (car end-point))))
	;; 	((= angle (/ pi 2)) 
	;; 		(setq increment (- (cadr end-point) (cadr start-point))))
	;; 	((= angle (* (/ pi 2) 3)) 
	;; 		(setq increment (- (cadr start-point) (cadr end-point))))
	;; )











	(terpri)
	(prompt "***") (terpri)
	(princ (/ (* angle 180) pi)) (terpri)
	(princ increment) (terpri)
	(prompt "***")




 


)



(defun fix-angle (start-point end-point / angle)
	(setq angle (angle start-point end-point))
	(if (> angle pi) (setq angle (- angle (* pi 2))))

	(cond 
		((and (>= angle (/ pi -4)) (< angle (/ pi 4)))
			(setq angle 0))
		((and (>= angle (/ pi 4)) (< angle (* (/ pi 4) 3)))
			(setq angle (/ pi 2)))
		((and (>= angle (* (/ pi 4) -3)) (< angle (/ pi -4)))
			(setq angle (* (/ pi 2) 3)))
		(T (setq angle pi))

	)




	;; (setq angle (/ (* angle 180) pi))
	(princ angle)
)





;;; rutines de cotes
(defun c:dimitri (/	pt1   pt2   pt3	  os	pt11  ang12 ang13 angap
			pap	conj  lent  pt4	  pt5	ptb   num   n	  pt6
		 )
	(command "_undo" "_begin")
	(setq pt1 (getpoint "Entreu primer punt de la líña de cotes: "))
	(terpri)
	(setq pt2 (getpoint pt1 "Entreu segon punt de la líña de cotes: "))
	(terpri)
	(setq pt3 (getpoint "A quin costat voleu la líña de cotes? "))
	(terpri)
	(setq pt3 (trans pt3 1 0))
	(command "_ucsfollow" "0")
	(setq ang12 (angle pt1 pt2))
	(setq pt11 (polar pt1 (+ ang12 (/ pi 2)) 1))
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(command "_ucs" "3" pt1 pt2 pt11)
	(setvar "osmode" os)
	(setq pt3 (trans pt3 0 1))
	(if (< (cadr pt3) 0)
		(setq angap (- ang12 (/ pi 2)))
		(setq angap (+ ang12 (/ pi 2)))
	)
	(command "_ucs" "_p")
	(setq pt1 (trans pt1 1 0))
	(setq pt2 (trans pt2 1 0))
	(command "_ucs" "_w")
	(setq pap (polar pt1 angap did))
	(setq conj (ssget "_f" (list pt1 pt2) (list (cons 0 "line"))))
	(setq lent (entget (ssname conj 0)))
	(setq pt4 (cdr (assoc 10 lent)))
	(setq pt5 (cdr (assoc 11 lent)))
	(setq ptb (inters pt1 pt2 pt4 pt5))
	(setq num (- (sslength conj) 1))
	(setq n 1)
	(repeat num
		(setq lent (entget (ssname conj n)))
		(setq pt4 (cdr (assoc 10 lent)))
		(setq pt5 (cdr (assoc 11 lent)))
		(setq pt6 (inters pt1 pt2 pt4 pt5))
		(setq os (getvar "osmode"))
		(setvar "osmode" 0)
		(command "_dimaligned" ptb pt6 pap)
		(setvar "osmode" os)
		(setq ptb pt6)
		(setq n (+ n 1))
	)
	(command "_ucs" "_p")
	(command "_undo" "_end")
)
;;;
(defun c:f3 (/ CONJ)
	(setq CONJ (ssget))
	(prompt "Sel.lecciona cotes per posar 3 decimals: ")
	(terpri)
	(command "_dimoverride" "acodec" 3 "" CONJ "")
)
;;;
(defun c:f2 (/ CONJ)
	(setq CONJ (ssget))
	(prompt "Sel.lecciona cotes per posar 2 decimals: ")
	(terpri)
	(command "_dimoverride" "acodec" 2 "" CONJ "")
)
;;; hdim acotació horitzontal
(defun c:hdim (/ pin pfi pvi dap pap os)
	(setq pin (getpoint "Entreu primer punt: "))
	(terpri)
	(setq pfi (getpoint "Entreu segon punt: "))
	(terpri)
	(while pfi
		(setq pvi (list (car pfi) (cadr pin)))
		(if	(> (car pfi) (car pin))
			(setq pap (list (car pin) (+ (cadr pin) did)))
			(setq pap (list (car pin) (- (cadr pin) did)))
		)
		(setq os (getvar "osmode"))
		(setvar "osmode" 0)
		(command "_dimlinear" pin pvi pap)
		(setvar "osmode" os)
		(setq pin pvi)
		(setq pfi (getpoint "Entreu nou punt: "))
		(terpri)
	)
)
;;;
(defun c:hdims (/ pin pfi pvi dap pap os)
	(setq pin (getpoint "Entreu primer punt: "))
	(terpri)
	(setq pfi (getpoint "Entreu segon punt: "))
	(terpri)
	(while pfi
		(setq pvi (list (car pfi) (cadr pin)))
		(setq pap (list (car pin) (+ (cadr pin) did)))
		(setq os (getvar "osmode"))
		(setvar "osmode" 0)
		(command "_dimlinear" pin pvi pap)
		(setvar "osmode" os)
		(setq pin pvi)
		(setq pfi (getpoint "Entreu nou punt: "))
		(terpri)
	)
)
;;;
(defun c:hdimi (/ pin pfi pvi dap pap os)
	(setq pin (getpoint "Entreu primer punt: "))
	(terpri)
	(setq pfi (getpoint "Entreu segon punt: "))
	(terpri)
	(while pfi
		(setq pvi (list (car pfi) (cadr pin)))
		(setq pap (list (car pin) (- (cadr pin) did)))
		(setq os (getvar "osmode"))
		(setvar "osmode" 0)
		(command "_dimlinear" pin pvi pap)
		(setvar "osmode" os)
		(setq pin pvi)
		(setq pfi (getpoint "Entreu nou punt: "))
		(terpri)
	)
)
;;; vdim acotació vertical
(defun c:vdim (/ pin pfi pvi pap os)
	(setq pin (getpoint "Entreu primer punt: "))
	(terpri)
	(setq pfi (getpoint "Entreu segon punt: "))
	(terpri)
	(while pfi
		(setq pvi (list (car pin) (cadr pfi)))
		(if	(> (cadr pfi) (cadr pin))
			(setq pap (list (- (car pin) did) (cadr pin)))
			(setq pap (list (+ (car pin) did) (cadr pin)))
		)
		(setq os (getvar "osmode"))
		(setvar "osmode" 0)
		(command "_dimlinear" pin pvi pap)
		(setvar "osmode" os)
		(setq pin pvi)
		(setq pfi (getpoint "Entreu nou punt: "))
		(terpri)
	)
)
;;;
(defun c:vdimd (/ pin pfi pvi pap os)
	(setq pin (getpoint "Entreu primer punt: "))
	(terpri)
	(setq pfi (getpoint "Entreu segon punt: "))
	(terpri)
	(while pfi
		(setq pvi (list (car pin) (cadr pfi)))
		(setq pap (list (+ (car pin) did) (cadr pin)))
		(setq os (getvar "osmode"))
		(setvar "osmode" 0)
		(command "_dimlinear" pin pvi pap)
		(setvar "osmode" os)
		(setq pin pvi)
		(setq pfi (getpoint "Entreu nou punt: "))
		(terpri)
	)
)
;;;
(defun c:vdime (/ pin pfi pvi pap os)
	(setq pin (getpoint "Entreu primer punt: "))
	(terpri)
	(setq pfi (getpoint "Entreu segon punt: "))
	(terpri)
	(while pfi
		(setq pvi (list (car pin) (cadr pfi)))
		(setq pap (list (- (car pin) did) (cadr pin)))
		(setq os (getvar "osmode"))
		(setvar "osmode" 0)
		(command "_dimlinear" pin pvi pap)
		(setvar "osmode" os)
		(setq pin pvi)
		(setq pfi (getpoint "Entreu nou punt: "))
		(terpri)
	)
)
;;; adim acotació alineada
(defun c:adim (/ pin pfi ang pap os)
	(setq pin (getpoint "Entreu primer punt: "))
	(terpri)
	(setq pfi (getpoint "Entreu segon punt: "))
	(terpri)
	(while pfi
		(setq ang (angle pin pfi))
		(setq pap (polar pin (+ ang (/ pi 2)) did))
		(setq os (getvar "osmode"))
		(setvar "osmode" 0)
		(command "_dimaligned" pin pfi pap)
		(setvar "osmode" os)
		(setq pin pfi)
		(setq pfi (getpoint "Entreu nou punt: "))
		(terpri)
	)
)
;;; aquesta fundió acota però amb un canvi d'ucs, d'aquesta
;;; manera el text apareix girat en el sentit de la cota
(defun angles ()
	(setq ang (angle p1 p2))
	(if (or (>= (/ pi 4) ang 0)
		(> (* 2 pi) ang (* 7 (/ pi 4)))
			)
		(progn
			(setq angx (* pi 2))
			(setq angy (/ pi 2))
		)
	)
	(if (or (> (* 3 (/ pi 2)) ang (* 5 (/ pi 4)))
		(>= (* 7 (/ pi 4)) ang (* 3 (/ pi 2)))
			)
		(progn
			(setq angx (* (/ pi 2) 3))
			(setq angy (* pi 2))
		)
	)
	(if (or (>= (* 5 (/ pi 4)) ang pi)
		(> pi ang (* 3 (/ pi 4)))
			)
		(progn
			(setq angx pi)
			(setq angy (* 3 (/ pi 2)))
		)
	)
	(if (or (> (/ pi 2) ang (/ pi 4))
		(>= (* 3 (/ pi 4)) ang (/ pi 2))
			)
		(progn
			(setq angx (/ pi 2))
			(setq angy pi)
		)
	)
)
(defun c:da (/ p1 p2 ang angx angy p10 p1x p1y p1i p2i p1a p2a pap os)
	(setq p1 (getpoint "Entreu primer punt: "))
	(terpri)
	(setq p2 (getpoint "Entreu segon: "))
	(terpri)
	(angles)
	(setq p10 p1)
	(setq p1x (polar p1 angx 1))
	(setq p1y (polar p1 angy 1))
	(setq p1 (trans p1 1 0))
	(setq p2 (trans p2 1 0))
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(command "_ucs" "_n" "_3" p10 p1x p1y)
	(setvar "osmode" os)
	(setq p1 (trans p1 0 1))
	(setq p2 (trans p2 0 1))
	(angles)
	(while p2
		(setq p1i (polar p1 angx 1))
		(setq p2i (polar p2 angy 1))
		(setq p2 (inters p1 p1i p2 p2i nil))
		(setq p1a (polar p1 angy dil))
		(setq p2a (polar p2 angy dil))
		(setq pap (polar p1a angy did))
		(setvar "osmode" 0)


		; (command "_dimlinear" p1a p2a pap)
		; (command "_DIM" "_AL" p1a p2a pap)


		(setvar "osmode" os)
		(setq p1 p2)
		(setq p2 (getpoint "Entreu nou punt <retorn per acabar>: "))
		(terpri)
	)
	(command "_ucs" "_p")
)
;;; aquesta en canvi acota normalment sense girar el text, no
;;; hi ha un canvi d'ucs 
(defun c:dh (/ p1 p2 ang angx angy p1i p2i p1a p2a pap os)
	(setq p1 (getpoint "Entreu primer punt: "))
	(terpri)
	(setq p2 (getpoint "Entreu segon punt: "))
	(terpri)
	(setq ang (angle p1 p2))
	(if (or (>= (/ pi 4) ang 0)
		(> (* 2 pi) ang (* 7 (/ pi 4))))
		(progn
			(setq angx (* pi 2))
			(setq angy (/ pi 2))
		)
	)
	(if (or (> (* 3 (/ pi 2)) ang (* 5 (/ pi 4)))
		(>= (* 7 (/ pi 4)) ang (* 3 (/ pi 2)))
			)
		(progn
			(setq angx (* (/ pi 2) 3))
			(setq angy (* pi 2))
		)
	)
	(if (or (>= (* 5 (/ pi 4)) ang pi)
		(> pi ang (* 3 (/ pi 4))))
		(progn
			(setq angx pi)
			(setq angy (* 3 (/ pi 2)))
		)
	)
	(if (or (> (/ pi 2) ang (/ pi 4))
		(>= (* 3 (/ pi 4)) ang (/ pi 2))
			)
		(progn
			(setq angx (/ pi 2))
			(setq angy pi)
		)
	)
;;;  (setq dis (distance p1 p2))
;;;  (setq disy (abs (* dis (sin ang))))
;;;  (setq disx (abs (* dis (cos ang))))
	(while p2
		(setq p1i (polar p1 angx 1))
		(setq p2i (polar p2 angy 1))
		(setq p2 (inters p1 p1i p2 p2i nil))
		(setq os (getvar "osmode"))
		(setvar "osmode" 0)
		(setq p1a (polar p1 angy dil))
		(setq p2a (polar p2 angy dil))
		(setq pap (polar p1a angy did))
		(command "_dimlinear" p1a p2a pap)
		(setvar "osmode" os)
		(setq p1 p2)
		(setq p2 (getpoint "Entreu nou punt <retorn per acabar>: "))
		(terpri)
	)
)
;;;
;;;
;;;
;;;GT.LSP gira 90 graus text cota
(defun c:gt (/ n ent lent ang)
	(setq n 0)
	(while (/= n 1)
		(while (not ent)
			(setq ent (entsel "Selecciona cota: "))
			(terpri)
		)
		(setq lent (entget (car ent)))
		(if	(= (cdr (assoc 0 lent)) "DIMENSION")
			(setq n 1)
			(progn
	(prompt "Cal que sigui una cota <Esc per sortir>: ")
	(terpri)
	(setq ent nil)
			)
		)
	)
	(setq ang (+ (cdr (assoc 53 lent)) (/ pi 2)))
	(setq lent (subst (cons 53 ang) (assoc 53 lent) lent))
	(entmod lent)
)



;; 
(prompt "dim.lsp successfully loaded!\n")
