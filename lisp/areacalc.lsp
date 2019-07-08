; areacalc.lsp
; 160725

; (defun *error* (str)
; 	(princ "Cagada: ") (princ str) (terpri)
; )

; Command call
(defun c:sup ()
	(boundary-area)
)

(defun c:sum ()
	(area-sum)
)

(defun c:pls (/ os)
	(setq os (getvar "OSMODE"))
	(setvar "OSMODE" 0)
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BEGIN")
	(polyline-area)
	(command "_UNDO" "_END")
	(setvar "CMDECHO" 1)
	(setvar "OSMODE" os)
)

(defun c:ad (/ os)
	(setq os (getvar "OSMODE"))
	(setvar "OSMODE" 0)
	(diagonal-area)
	(setvar "OSMODE" os)
)


; BOUNDARY-AREA
; main function that computes area by
; picking interior point of a closed space
(defun boundary-area (/ pnt ent area str1 str2)
	(setvar "cmdecho" 0)
	(command "_undo" "_begin")
	(setq ent (pick-boundary))
	(area-calc ent)

	(entdel ent)

	;; (setq str1 "S.Útil ")
	(setq str1 "")
	(setq str2 "m2")
	; (setq str2 "")
	(draw-text str1 str2 area pnt)
	(command "_undo" "_end")
	(setvar "cmdecho" 1)
	(princ)
)


; takes an entity name
; returns the type of given entity from its list
(defun get-type (ent-name)
	(setq ent-list (entget ent-name))
	(setq ent-type (cdr (assoc 0 ent-list)))
)


; aux function that makes boundary by picking
; interior point and assings entity name to var ent
(defun pick-boundary (/ point-ent output)
	(command "_POINT" "0,0,0")
	(setq point-ent (entlast))
	(while (= (get-type (entlast)) "POINT")
		(setq pnt (getpoint "Pick boundary interior point: "))
		(command "_-boundary" pnt "")
		(if (= (get-type (entlast)) "POINT")
			(prompt "\nBoundary not valid!")
		)
	)
	(entdel point-ent)
	(setq output (entlast))
)


; aux function that takes a closed entity, 
; computes its area and assings value to var area
(defun area-calc (ent)
	(command "_area" "_e" ent)
	(setq area (getvar "area"))
	; (command "_-LAYER" "_N" "z" "_C" "3" "z" "")
	; (command "_CHANGE" (entlast)  "" "_P" "_LA" "_plsup" "")
	; (entdel ent)
)


; aux function that takes two strings, a float
; and a point. Concatenates them in a full string,
; and draws text centered in the point pnt
(defun draw-text (str1 str2 val pnt / txt txtlen dimzin)
	(setq dimzin (getvar "dimzin"))
	(setvar "dimzin" 0)
	(setq txt (rtos val 2 2))
	(setq txtlen (strlen txt))
	(setq txt (strcat str1 (substr txt 1 (- txtlen 3))
		"," (substr txt (- txtlen 1) 2) str2))
	(command "_text" "_m" pnt "" txt)
	(setvar "dimzin" dimzin)
)


; AREA-SUM
; main function that computes the sum of
; all numeric values of selected texts
(defun area-sum (/ total txtgroup txtgrouplen counter nent 
				lent txt val pnt str1 str2)
	(setvar "cmdecho" 0)
	(command "_undo" "_begin")
	(setq total 0)
	(setq txtgroup (select-txt))
	(setq txtgrouplen (sslength txtgroup))
	(setq counter 0)
	(repeat txtgrouplen
		(setq nent (ssname txtgroup counter))
		(setq lent (entget nent))
		(setq txt (cdr (assoc 1 lent)))
		(setq val (txt-val txt))
		(setq total (+ total val))
		(setq counter (+ counter 1))
	)
	(setq pnt (getpoint "Pick point to place result: "))
	(setq str1 "Total.")
	(setq str2 "m2")
	(draw-text str1 str2 total pnt)
	(command "_undo" "_end")
	(setvar "cmdecho" 1)	
	(princ)
)

; aux function, select a group of text entities
(defun select-txt (/ text-group)
  (prompt "Select objects: ")
  (setq text-group (ssget '((-4 . "<or") (0 . "TEXT") 
  	(0 . "MTEXT") (-4 . "or>"))))
)



; aux function, extracts numeric value of text
(defun txt-val (txt / txtlen char charstr startchar str-val)
	(setq str-val 0)
	(setq txt (acet-str-replace "," "." txt))
		; commas to points so decimal is well interpreted
	(setq txtlen (strlen txt))
	(setq char 1)
	(while (<= char txtlen)
		(setq charstr (substr txt char 1))
		(if (wcmatch charstr "#")
			(progn
				(setq startchar char)
				(setq str-val (atof (substr txt startchar)))
				(setq char txtlen)
			)
		)
		(setq char (+ char 1))
	)
	(eval str-val)
)

(defun polyline-area (/ pl-group pl-group-len pl-index)
	(setq pl-group (ssget '((0 . "LWPOLYLINE"))))
		; TODO: check polyline has 3 verts at least
	(setq pl-group-len (sslength pl-group))
	(setq pl-index 0)
	(repeat pl-group-len
		(setq pl-name (ssname pl-group pl-index))
		(setq pl-list (entget pl-name))
		(setq pl-vert (get-pl-vertices pl-list))
		(setq pl-area (area-calc pl-name))
		(setq txt-point (get-centroid pl-vert))
		(setq str1 "Sup.")
		(setq str2 "m2")
		(draw-text "" str2 pl-area txt-point)
		(setq pl-index (+ pl-index 1))
	)
)

(defun diagonal-area (/ pt1 pt4 pt2 pt3)
	(setq pt1 (getpoint "Pick first corner: "))
	(terpri)
	(setq pt4 (getpoint pt1 "Pick second corner: "))
	(terpri)
	(setq pt2 (list (car pt4) (cadr pt1)))
	(setq pt3 (list (car pt1) (cadr pt4)))
	(command "_AREA" pt1 pt2 pt4 pt3 "")
)

;
(prompt "areacalc.lsp successfully loaded!\n")
