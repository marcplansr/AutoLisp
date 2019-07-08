; select.lsp
; 171018

; Select helper functions

; generic select objects func
(defun select-objects (/ output)
	(while (not (setq output (ssget)))
		(if (= output nil)
				(prompt "\nNothing selected, try again!")
		)
	)
	(princ output)
)

;
(prompt "select.lsp successfully loaded!\n")
;




