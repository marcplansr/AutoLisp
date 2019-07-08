; w_var.lsp
; 180709
; saves permanent global var values

; macros
(defun c:VAR () (display-vars)) 	; prints var values
(defun c:DEF () (vars-def)) 		; sets default var values
(defun c:PGPAN () (page-pan-var))	; page pan vars
(defun c:SALT () (page-pan-var))
(defun c:DIT () (dit-var-input))	; text distance var
(defun c:GAP () (wall-gap-input))	; wall gap var
(defun c:LLM () (wall-gap-input))
(defun c:WID () (width-input))		; wall width var
(defun c:GRX () (width-input))
(defun c:DIS () (gap-dist-input))	; gap dist from wall end var
(defun c:EXC () (gap-dist-input))
(defun c:CLR () (clearance-input))	; door clearance var
(defun c:PAS () (clearance-input))
(defun c:FRM () (frame-input))		; frame length var
(defun c:MRC () (frame-input))
(defun c:LEA () (leaf-input))		; door leaf width var
(defun c:FLL () (leaf-input))
(defun c:PDI () (pnt-dist-input))
(defun c:MOB () (mob-dist-input))	; move objects distance var

; (defun c:DIT () (leaf-input))		;;;DID variable distància de separació aplicació de cotes
; (defun c:DIL () (leaf-input))       	;;;DIL variable distància de separació linia cotes


; gets *h-dist* *v-dist* vars from distance between two
; picked points, and calls write function to save them
(defun page-pan-var ( / pt1 pt2 h-temp v-temp var)
	(setq pt1 (getpoint "Pick first point : ")) (terpri)
	(setq pt2 (getpoint pt1 "Pick second point : ")) (terpri)
	(setq h-temp *h-dist*)
	(setq v-temp *v-dist*)
	(setq *h-dist* (- (car pt2) (car pt1)))
	(setq *v-dist* (- (cadr pt2) (cadr pt1)))
	(if (= *h-dist* nil)
		(setq *h-dist* h-temp))
	(if (= *v-dist* nil)
		(setq *v-dist* v-temp))
	(display-vars) (write-vars) (princ)
)


; input of text distance var
(defun dit-var-input ( / temp var)
	(vars-seg)
	(setq temp *dit*)
	(setq *dit* (getreal (strcat "Enter text distance <" 
		(rtos temp 2 4) ">: ")))
	(if (= *dit* nil)(setq *dit* temp))
	(display-vars) (write-vars) (princ)
)


; input of wall gap length
(defun wall-gap-input ( / temp var)
	(vars-seg)
	(setq temp *wall-gap*)
	(setq *wall-gap* (getreal (strcat "Enter wall gap length <" 
		(rtos temp 2 4) ">: ")))
	(if (= *wall-gap* nil)(setq *wall-gap* temp))
	(setq *clearance* (- *wall-gap* (* 2 *frame*)))
	(display-vars) (write-vars) (princ)
)


; input of wall width
(defun width-input ( / temp var)
	(vars-seg)
	(setq temp *width*)
	(setq *width* (getreal (strcat "Enter wall width <" 
		(rtos temp 2 4) ">: ")))
	(if (= *width* nil)(setq *width* temp))
	(display-vars) (write-vars) (princ)
)


; input of distance between gap and wall end
(defun gap-dist-input ( / temp var)
	(vars-seg)
	(setq temp *gap-dist*)
	(setq *gap-dist* (getreal (strcat "Enter dist from wall end <" 
		(rtos temp 2 4) ">: ")))
	(if (= *gap-dist* nil)(setq *gap-dist* temp))
	(display-vars) (write-vars) (princ)
)


; input of carpentry clearance
(defun clearance-input ( / temp var)
	(vars-seg)
	(setq temp *clearance*)
	(setq *clearance* (getreal (strcat "Enter clearance <" 
		(rtos temp 2 4) ">: ")))
	(if (= *clearance* nil)(setq *clearance* temp))
	(setq *wall-gap* (+ *clearance* (* 2 *frame*)))
	(display-vars) (write-vars) (princ)
)


; input frame length
(defun frame-input ( / temp var)
	(vars-seg)
	(setq temp *frame*)
	(setq *frame* (getreal (strcat "Enter frame length <" 
		(rtos temp 2 4) ">: ")))
	(if (= *frame* nil)(setq *frame* temp))
	(setq *clearance* (- *wall-gap* (* 2 *frame*)))
	(display-vars) (write-vars) (princ)
)


; carpentry leaf width input
(defun leaf-input ( / temp var)
	(vars-seg)
	(setq temp *leaf*)
	(setq *leaf* (getreal (strcat "Enter leaf width <" 
		(rtos temp 2 4) ">: ")))
	(if (= *leaf* nil)(setq *leaf* temp))
	(display-vars) (write-vars) (princ)
)


; input of point distance var
(defun pnt-dist-input ( / temp var)
	(vars-seg)
	(setq temp *pnt-dist*)
	(setq *pnt-dist* (getreal (strcat "Enter point distance <" 
		(rtos temp 2 4) ">: ")))
	(if (= *pnt-dist* nil)(setq *pnt-dist* temp))
	(display-vars) (write-vars) (princ)
)


; input of distance to move objects, same dist for x and y axes
(defun mob-dist-input ( / temp var)
	(vars-seg)
	(setq temp *mob-dist*)
	(setq *mob-dist* (getreal (strcat "Enter dist to move objects <" 
		(rtos temp 2 4) ">: ")))
	(if (= *mob-dist* nil)(setq *mob-dist* temp))
	(display-vars) (write-vars) (princ)
)



; prints current permanent global vars on screen
(defun display-vars ( / h-len v-len)
	(vars-seg)
	(setq h-len (strlen (itoa (fix *h-dist*))))
	(setq v-len (strlen (itoa (fix *v-dist*))))
	(setq pnt-len (strlen (itoa (fix *pnt-dist*))))
	(setq h-len (- 7 h-len))
	(setq v-len (- 7 v-len))
	(setq pnt-len (- 7 pnt-len))

	(prompt (strcat 
		"*h-dist*    = " (rtos *h-dist* 2 h-len) "\n"
		"*v-dist*    = " (rtos *v-dist* 2 v-len) "\n"
		"*dit*       = " (rtos *dit* 2 6) "\n"
		"*wall-gap*  = " (rtos *wall-gap* 2 6) "\n"
		"*width*     = " (rtos *width* 2 6) "\n"
		"*gap-dist*  = " (rtos *gap-dist* 2 6) "\n"
		"*clearance* = " (rtos *clearance* 2 6) "\n"
		"*frame*     = " (rtos *frame* 2 6) "\n"
		"*leaf*      = " (rtos *leaf* 2 6) "\n"
		"*pnt-dist*  = " (rtos *pnt-dist* 2 pnt-len) "\n"
		"*mob-dist*  = " (rtos *mob-dist* 2 6)))
	(princ)
)


; opens var.lsp and writes permanent global vars values
(defun write-vars ( / file )
	(vars-seg)
	(setq file (open "Bricsys/BricsCAD/V18x64/en_US/Support/Lisp/var.lsp" "w"))
	(write-line "; var.lsp" file)
	(write-line (strcat"; " 
		(menucmd "m=$(edtime,$(getvar,date),yymodd)")) file)
	(write-line "; permanent global var values storage" file)
	(write-line " " file)
	(write-line "; page pan global vars" file) 
	(write-line (strcat "(setq *h-dist* " (rtos *h-dist* 2 5) ")") file)
	(write-line (strcat "(setq *v-dist* " (rtos *v-dist* 2 5) ")") file)
	(write-line " " file)
	(write-line "; text vars" file)
   	(write-line (strcat "(setq *dit* " (rtos *dit* 2 6) ")") file)
	(write-line " " file)
	(write-line "; carpentry global vars" file)
	(write-line (strcat "(setq *wall-gap* " (rtos *wall-gap* 2 6) ")") file)
	(write-line (strcat "(setq *width* " (rtos *width* 2 6) ")") file)
	(write-line (strcat "(setq *gap-dist* " (rtos *gap-dist* 2 6) ")") file)
	(write-line (strcat "(setq *clearance* " (rtos *clearance* 2 6) ")") file)
	(write-line (strcat "(setq *frame* " (rtos *frame* 2 6) ")") file)
	(write-line (strcat "(setq *leaf* " (rtos *leaf* 2 6) ")") file)
	(write-line " " file)
	(write-line "; geocomp vars" file)
  	(write-line (strcat "(setq *pnt-dist* " (rtos *pnt-dist* 2 5) ")") file)
	(write-line " " file)
	(write-line "; move objects distance var" file)
	(write-line (strcat "(setq *mob-dist* " (rtos *mob-dist* 2 6) ")") file)
	(write-line "(princ)" file)
	(write-line " " file)
	(write-line ";" file) 
	(write-line "(prompt \"var.lsp successfully loaded!\\n\")" file)
	(close file)
)


; sets default var values if nil
(defun vars-seg ()
	(if (= *h-dist* nil) (setq *h-dist* 60))
	(if (= *v-dist* nil) (setq *v-dist* 45))
	(if (= *dit* nil) (setq *dit* 0.02))
	(if (= *wall-gap* nil) (setq *wall-gap* 0.9))
	(if (= *width* nil) (setq *width* 0.15))
	(if (= *gap-dist* nil) (setq *gap-dist* 0.025)) 
	(if (= *clearance* nil) (setq *clearance* 0.8))
	(if (= *frame* nil) (setq *frame* 0.05))
	(if (= *leaf* nil) (setq *leaf* 0.05))
	(if (= *pnt-dist* nil) (setq *pnt-dist* 1))
	(if (= *mob-dist* nil) (setq *mob-dist* 0.025))
)


; sets var default values
(defun vars-def ()
	(setq *h-dist* 60)
	(setq *v-dist* 45)
	(setq *dit* 0.02)
	(setq *wall-gap* 0.9)
	(setq *width* 0.15)
	(setq *gap-dist* 0.025)
	(setq *clearance* 0.8)
	(setq *frame* 0.05)
	(setq *leaf* 0.05)
	(setq *pnt-dist* 1)
	(setq *mob-dist* 0.025)
)

;
(prompt "w_var.lsp successfully loaded!\n")

