; helpers.lsp
; 180510







(defun lab (prec samples / rand-list rand-output score)
	(setq rand-list (list))
	(repeat prec
		(setq rand-list (append rand-list '(0)))
	)
	(print-list rand-list)
	(repeat samples
		(setq rand-output (random prec))
		(setq score (nth rand-output rand-list))
		(setq score (+ score 1))
		(setq rand-list (subst-in-list score rand-list rand-output))
	)
	(print-list rand-list)

)


; Function that rounds a given real number, num,
; with a number of decimal places defined by the
; second argument prec, an integer.
(defun round-num (num prec / quotient)
	(setq quotient (* num (expt 10 prec)))
	(setq remainder (fix (* (rem quotient 1) 10)))
	(setq quotient (fix quotient))
	(if (> remainder 5)
		(setq quotient (+ quotient 1))
	)
	(setq quotient (/ quotient (float (expt 10 prec))))
	(princ quotient)
)


; returns 2d distance between two points 
; given their coords list
(defun points-dist (point1 point2)
	(sqrt (+ (expt (- (car point2) (car point1)) 2) 
		(expt (- (cadr point2) (cadr point1)) 2)))
)


; returns a random value between two given limits
; both limits inclusive
(defun randrange (limit1 limit2 decimal-places / 
		range precision)
	(setq range (abs (- limit1 limit2)))
	(setq precision (* range (expt 10 decimal-places)))
	(+ (min limit1 limit2) 
		(* (/ (atof (itoa (random (+ precision 1)))) precision) 
			(abs (- limit1 limit2))))
)


; LISTS FUNCTIONS
; SUB-LIST. Takes original list, start point of sublist
; and length of sublist. Returns sublist, if length is 0
; returns sublist from start point till end of list.
; Start point index inclusive. First list index is 0
(defun sub-list (full-list index sub-list-length / 
		sub-list full-list-length)
	(setq sub-list (list))
	(setq full-list-length (length full-list))
	(cond ; error handling
		((> index (- full-list-length 1))
			(prompt "error: start index exceeds list size!")
			(exit))
		((> sub-list-length (- full-list-length index))
			(prompt "error: sublist length exceeds list size!")
			(exit))
		((or (< index 0) (< sub-list-length 0))
			(prompt "error: negative values not valid!")
			(exit))
	)
	(if (= sub-list-length 0)
		(setq sub-list-length 
			(- full-list-length index))
	)
	(repeat sub-list-length
		(setq sub-list (append sub-list 
			(list (nth index full-list))))
		(setq index (1+ index))
	)
	(princ sub-list)
)


; substitute element in list by index, if
; index is negative substitutes last element in list
(defun subst-in-list (new-element full-list index / 
		list-length	left-list right-list index)
	(setq list-length (length full-list))
	(cond ; error handling
		((> index (- list-length 1))
			(prompt "error: subst index exceeds list size!")
			(exit))
	)
	(setq left-list (list))
	(setq right-list (list))
	(if (< index 0)
		(setq index (- list-length 1))
	)
	(if (> index 0)
		(setq left-list (sub-list full-list 0 index))
	)
	(if (< index (- list-length 1))
		(setq right-list 
			(sub-list full-list (+ index 1) (- list-length (+ index 1))))
	)
	(setq full-list (append left-list (list new-element) right-list))
)


(defun print-list (in-list / output-str index leading-zeros)
	(setq output-str "")
	(setq index 0)
	(foreach element in-list
		(cond
			((< index 10) (setq leading-zeros "00"))
			((< index 100) (setq leading-zeros "0"))
			( T (setq leading-zeros ""))
		)
		(setq output-str (strcat output-str "Item " leading-zeros
			(itoa index) ": " (itoa element) " \n"))
		(setq index (1+ index))
	)
	(princ output-str) (terpri)
)


;
(prompt "helpers.lsp successfully loaded!\n")





