; block.lsp
; 161030

; quick block creation takes as argument a group of objects,
; if not picked, insert point takes view center coordinates,
; block name is its creation date and time (yymoddhhmmss)
(defun make-block (conj / nblc pins os)
	(setq pins (getpoint "Pick insert point: "))
	(terpri)
	(if (= pins nil)
  		(setq pins (getvar "VIEWCTR"))
	)
	(setq nblc (menucmd "m=$(edtime,$(getvar,date),yymoddhhmmss)"))
	(setq os (getvar "OSMODE"))
	(setvar "OSMODE" 0)
	(command "_-BLOCK" nblc pins conj "")
	(command "_-INSERT" nblc pins "" "" "")
	(command "_CHANGE" "_LAST" "" "_P" "_LA" "0" "")
	(setvar "OSMODE" os)
)

; select objects and calls make-block function
(defun c:N (/ conj)
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BEGIN")
	(setq conj (ssget))
	(terpri)
	(make-block conj)
	(command "_UNDO" "_END")
	(setvar "CMDECHO" 1)
	(princ)
)

; select objects and calls make-block function,
; prev changes objects lo layer 0 and props to bylayer
(defun c:N0 (/ conj)
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BEGIN")
	(setq conj (ssget))
	(terpri)
	(to-layer-0 conj); layer.lsp
	(to-bylayer conj); layer.lsp
	(make-block conj)
	(command "_UNDO" "_END")
	(setvar "CMDECHO" 1)
	(princ)
)


;
(prompt "block.lsp loaded successfully!!!\n")qs
