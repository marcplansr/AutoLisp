; layer.lsp
; 161030

; takes a group of objects as argument and changes
; them to layer z, prev created if it did not exist
(defun to-layer-z (conj /)
	(command "_-LAYER" "_N" "z" "C" "3" "z" "")
	(command "_CHANGE" conj "" "_P" "_LA" "z" "")
)

; takes a group of objects as argument 
; and changes them to layer 0
(defun to-layer-0 (conj /)
	(command "_CHANGE" conj "" "_P" "_LA" "0" "")
)

; takes a group of objects as argument 
; and changes main properties to bylayer
(defun to-bylayer(conj /)
	(command "_CHANGE" conj "" "_P" "_C" "bylayer" "")
	(command "_CHANGE" conj "" "_P" "_LT" "bylayer" "")
	(command "_CHANGE" conj "" "_P" "_S" "1" "")
)

; select objects and calls to-layer-z
(defun c:AZ (/ conj)
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BEGIN")
	(setq conj (ssget))
	(terpri)
    (to-layer-z conj)
    (command "_UNDO" "_END")
    (setvar "CMDECHO" 1)
    (princ)
)

; select objects, duplicates them and calls to-layer-z
(defun c:CZ (/ conj)
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BEGIN")
	(setq conj (ssget))
	(command "_COPY" conj "" "0,0,0" "0,0,0")
	(terpri)
    (to-layer-z conj)
    (command "_UNDO" "_END")
    (setvar "CMDECHO" 1)
    (princ)
)

; select objects and calls to-layer-0
(defun c:A0 (/ conj)
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BEGIN")
	(setq conj (ssget))
	(terpri)
    (to-layer-0 conj)
    (command "_UNDO" "_END")
    (setvar "CMDECHO" 1)
    (princ)
)

; select objects and calls to-bylayer
(defun c:BL (/ conj)
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BEGIN")
	(setq conj (ssget))
	(terpri)
    (to-bylayer conj)
    (command "_UNDO" "_END")
    (setvar "CMDECHO" 1)
    (princ)
)


; layer isolate
(defun c:IO	(/ cadn conj lay)
	(command "_UNDO" "_BEGIN")
	(setq cadn (getstring "Enter filter string to isolate layers: "))
	(setq cadn (strcat "*" cadn "*"))
	(setq conj (ssget "x" (list (cons 8 cadn))))
	(setq lay (cdr (assoc 8 (entget (ssname conj 0)))))
	(command "_-LAYER" "_ON" cadn "_T" cadn "_S" lay 
			"_OFF" "*" ""	"_ON" cadn	"")
	(while cadn 
		(setq cadn (getstring 
				"Any other string to add to filter selection? : "))
		(setq cadn (strcat "*" cadn "*"))
		(if (= cadn "* *") (setq cadn nil))
		(if (/= cadn nil)
			(command "_-LAYER" "_ON" cadn "_T" cadn ""))
	)
	(command "_UNDO" "_END")
)


; layer freeze
(defun c:my-layer-freeze (/ ent lay str)
	(command "_UNDO" "_BEGIN")
	(setq ent (entsel))
	(terpri)
	(if (/= ent nil)
		(progn
			(setq lay (cdr (assoc 8 (entget (car ent)))))
			(if (= lay (getvar "CLAYER"))
				(progn
					(prompt (strcat "Can not freeze current layer: " lay))
					(exit)
				)
				(command "_-LAYER" "_F" lay "")
			)
		)
		(progn
			(setq str (getstring "Enter filter string to freeze layers: "))
			(command "_-LAYER" "_F" (strcat "*" str "*") "")
		)
	)
	(command "_UNDO" "_END")
)





; ; turns on and unfreezes layers with filter possibility
; (defun c:OTX (/ flt txt)
;   (terpri)
;   (setq flt (getstring "Enter filter string: "))
;   (setq txt (strcat flt "*"))
;   (command "_-layer" "_t" txt "_on" txt "" ^c^c)
; )

; ;;; congela i desactiva capes amb possibilitat de filtre
; (defun c:fofx (/ flt txt)
;   (terpri)
;   (setq flt (getstring "entra cadena de filtre: "))
;   (setq txt (strcat flt "*"))
;   (command "_-layer" "_f" txt "_off" txt "" ^c^c)
; )

; ;;; copia objectes a la capa actual
; (defun c:xc (/ conj lay os)
;   (setq conj (ssget))
;   (prompt "sel.lecciona objectes copiar a la capa actual: ")
;   (terpri)
;   (setq lay (getvar "clayer"))
;   (command "_copy" conj "" "0,0" "0,0")
;   (command "_change" conj "" "_p" "_la" lay "")
; )

; ;;; descongela una capa determinada amb filtre de nom
; (defun c:tx (/ flt txt)
;   (terpri)
;   (setq flt (getstring "entra cadena de filtre: "))
;   (setq txt (strcat flt "*"))
;   (command "_-layer" "_t" txt "" ^c^c)
; )
; ;;; congela una capa determinada amb filtre de nom
; (defun c:fx (/ flt txt)
;   (terpri)
;   (setq flt (getstring "entra cadena de filtre: "))
;   (setq txt (strcat flt "*"))
;   (command "_-layer" "_f" txt "" ^c^c)
; )
; ;;; activa una capa determinada amb filtre de nom
; (defun c:onx (/ flt txt)
;   (terpri)
;   (setq flt (getstring "entra cadena de filtre: "))
;   (setq txt (strcat flt "*"))
;   (command "_-layer" "_on" txt "" ^c^c)
; )
; ;;; desactiva una capa determinada amb filtre de nom
; (defun c:ofx (/ flt txt)
;   (terpri)
;   (setq
;     flt	(getstring "entra cadena de filtre <retorn per designar>: ")
;   )
;   (setq txt (strcat flt "*"))
;   (command "_-layer" "_off" txt "" ^c^c)
; )
; ;;; desbloqueja una capa determinada amb filtre de nom
; (defun c:ux (/ flt txt)
;   (terpri)
;   (setq flt (getstring "entra cadena de filtre: "))
;   (setq txt (strcat flt "*"))
;   (command "_-layer" "_u" txt "" ^c^c)
; )
; ;;; bloqueja una capa determinada amb filtre de nom
; (defun c:lox (/ flt txt)
;   (terpri)
;   (setq flt (getstring "entra cadena de filtre: "))
;   (setq txt (strcat flt "*"))
;   (command "_-layer" "_lo" txt "" ^c^c)
; )












;
(prompt "layer.lsp successfully loaded!\n")

