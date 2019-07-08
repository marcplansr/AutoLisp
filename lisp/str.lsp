; str.lsp
; 170907
; modify strings


; command calls
(defun c:TTH () (txt01 "Habitació"))
(defun c:TTR () (txt01 "Rebedor"))
(defun c:TTT () (txt01 "Traster"))
(defun c:TTB () (txt01 "C.Higiènica"))
(defun c:TTC () (txt01 "Cuina"))
(defun c:TTS () (txt01 "Estar-Menjador"))
(defun c:TTP () (txt01 "Passadís"))
(defun c:TTD () (txt01 "Distribuïdor"))
(defun c:TTG () (txt01 "Galeria"))

; TXT01
; string modification Trafalgar project
(defun txt01 ( room / total txtgroup txtgrouplen
		counter nent lent txt-str txt-pos txt-val)
	(setvar "cmdecho" 0)
	(command "_undo" "_begin")
	(setq total 0)
	(setq txtgroup (select-txt)) ;areacalc.lsp
	(setq txtgrouplen (sslength txtgroup))
	(setq counter 0)
	(repeat txtgrouplen
		(setq nent (ssname txtgroup counter))
		(setq lent (entget nent))
		(setq txt-str (cdr (assoc 1 lent)))
		(setq txt-pos (cdr (assoc 10 lent)))
		(setq txt-str (substr txt-str (- (strlen txt-str) 6) 5))
		(setq str-val (txt-val txt-str)) ;areacalc.lsp
		(draw-text01 room str-val txt-pos)
		(setq counter (+ counter 1))
	)
	(command "_undo" "_end")
	(setvar "cmdecho" 1)	
	(princ)
)

; DRAW-TEXT01
; takes string for first line text, a float for 
; second line text, and a placement point
; draws text according to desired format
(defun draw-text01 (txt1 val pnt / txt2 pnt1 pnt2 os conj) ;;;conj move
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
   (setq txt2 (rtos val 2 2))
   (setq txt2 (acet-str-replace "." "," txt2))
	(setq txt2 (strcat "SU." txt2 "m2"))
	(setq pnt1 (list (car pnt) (+ (cadr pnt) (/ *dit* 2)) (caddr pnt)))
	(setq pnt2 (list (car pnt) (- (cadr pnt) (/ *dit* 2)) (caddr pnt)))
	(setq conj (ssadd)) ;;;move
	(command "_text" "_c" pnt1 "" txt1)
	(setq conj (ssadd (entlast) conj)) ;;;move
	(command "_text" "_c" pnt2 "" txt2)
	(setq conj (ssadd (entlast) conj)) ;;;move
	(command "_MOVE" conj "" pnt pause) ;;;move
	(setvar "osmode" os)
	(princ)
)





(prompt "str.lsp successfully loaded!\n")

