; delaunay.lsp
; 170313

; Delaunay triangulation

; PARABOLOID
; takes point list (x, y)
; returns x^2 + y^2
(defun paraboloid (pnt)
	(+ (expt (nth 0 pnt) 2)
		(expt (nth 1 pnt) 2))
)


; FUNC1
; takes layer name
; returns list of points coordinates in given layer 
(defun func1 (lay / grp i nent pnts pnt)
	(if (setq grp (ssget "X" 
			(list '(0 . "POINT") (cons 8 lay))))
		(progn
			(setq i 0)
			(while (setq nent (ssname grp i))
				(setq pnt (value 10 nent))
				(setq pnts (cons (list 
					(nth 0 pnt)
					(nth 1 pnt)
					(paraboloid pnt)) pnts))
				(setq i (+ i 1)))
			pnts
		)
	)
)


; VALUE helper function
; takes entity list code and entity name
; returns code value in given entity name list
(defun value (code entname)
	(cdr (assoc code (entget entname)))
)


; VECTOR helper function
; takes two 3d points, pnt1 and pnt2
; returns the vector from point pnt1 to pnt2
(defun vector (pnt1 pnt2)
	(list 
		(- (nth 0 pnt1) (nth 0 pnt2))
		(- (nth 1 pnt1) (nth 1 pnt2))
		(- (nth 2 pnt1) (nth 2 pnt2))
	)
)


; SCALAR-PROD helper function
; takes two vectors, vct1 and vct2
; returns their scalar product 
(defun scalar-prod (vct1 vct2)
	(+ (* (nth 0 vct1) (nth 0 vct2))
		(* (nth 1 vct1) (nth 1 vct2))
		(* (nth 2 vct1) (nth 2 vct2)))
)


; VECTOR-PROD helper function
; takes two vectors, vct1 and vct2
; returns their scalar product 
(defun vector-prod (vct1 vct2)
	(list
		(- (* (nth 1 vct1) (nth 2 vct2))
			(* (nth 2 vct1) (nth 1 vct2)))
		(- (* (nth 2 vct1) (nth 0 vct2))
			(* (nth 0 vct1) (nth 2 vct2)))
		(- (* (nth 0 vct1) (nth 1 vct2))
			(* (nth 1 vct1) (nth 0 vct2)))
	)
)


; PLACE-3D
; takes two points and a vector, pnt1 pnt2 vct
; returns scalar product of vector vct 
; and vector from point pnt1 to pnt2
(defun place-3d (pnt1 pnt2 vct / test)
	(setq test (scalar-prod (vector pnt1 pnt2) vct))
	(or (equal test 0.0 1e-5) (< test 0.0))
)


; TRIM
; takes a list as argument
; returns the given list without the last element
(defun trim (list)
	(reverse (cdr (reverse list)))
)









(defun c:lab (/ vct sca)
	(rec '(1 2 3 4 5 6 7 8 9))


	; (place-3d '(3 4 6) '(2 1 2) '(7 2 6))


	; (setq vct (vector '(2 1 2) '(3 4 6)))
	; (print vct)
	; (setq sca (scalar-prod vct '(7 2 6)))
	; (print sca)
	; (or (equal sca 0.0 1e-5) (< sca 0.0))








	; (setq pt (getpoint "Pick point: "))
	; (prompt (rtos (paraboloid pt) 2 4))

	; (setq ent (entsel))
	; (setq nent (car ent))
	; (setq lent (entget nent))
	; (setq val (value 10 nent))

	; (setq grp1 (ssget "X" 
	; 	(list (cons 0 "LINE") (cons 8 "points"))))
	; (setq len (sslength grp1))l

	; (setq grp (ssget))
	; (setq pnt )

	; (func1 "points")

	; (setq pnt1 (getpoint "Pick point: "))
	; (print pnt1)
	; (setq pnt2 (getpoint "Pick point: "))
	; (print pnt2)
	; (scalar-prod pnt1 pnt2)

	; (vector-prod '(4 6 7) '(5 8 9))



)




























;
(prompt "delaunay.lsp successfully loaded!\n")