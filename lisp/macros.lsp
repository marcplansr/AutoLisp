; macros.lsp
; 161122
; macros and keyboard shortcuts

; reload custom lisp files
(defun c:RELOAD () (load "on_start.lsp"))


; aliases overrays
(defun c:3DF () (command "_3DFACE"))
(defun c:AA () (command "_AREA"))
(defun c:B () (command "_BLOCK"))
(defun c:BD () (command "_-BOUNDARY"))
(defun c:BK () (command "_BREAK"))
(defun c:C () (command "_COPY" (ssget) "" "_M"))
(defun c:CC () (command "_CIRCLE"))
(defun c:D () (command "_DIST"))
(defun c:DI () (command "_DIVIDE"))
(defun c:DD () (command "_DDEDIT"))
(defun c:DW () (command "_DVIEW"))
(defun c:E () (command "_ERASE"))
(defun c:ET () (command "_EXTEND"))
(defun c:F () (command "_FILLET"))
(defun c:HD () (command "_HIDE"))
(defun c:I () (command "_DDINSERT"))
(defun c:L () (command "_LINE"))
(defun c:LA () (command "_-LAYER"))
(defun c:LE () (command "_LENGTHEN"))
(defun c:LI () (command "_LIST"))
(defun c:LTS () (command "_LTSCALE"))
(defun c:M () (command "_MOVE"))
(defun c:MP () (command "_MATCHPROP"))
(defun c:MI () (command "_MIRROR"))
(defun c:MR () (command "_MIRROR"))
(defun c:MS () (command "_MSPACE"))
(defun c:MT () (command "_MTEXT"))
(defun c:O () (command "_OFFSET"))
(defun c:OP () (command "_OPEN"))
(defun c:OS () (command "_OSNAP"))
(defun c:P () (command "_-PAN"))
(defun c:PD () (command "_PEDIT"))
(defun c:PL () (command "_PLINE"))
(defun c:PN () (command "_POINT"))
(defun c:PNN () (command "_POINT" "_M"))
(defun c:PS () (command "_PSPACE"))
(defun c:QS () (command "_QSAVE"))
(defun c:R () (command "_REDRAW"))
(defun c:RC () (command "_RECTANG"))
(defun c:RG () (command "_REGEN"))
(defun c:RO () (command "_ROTATE"))
(defun c:RT () (command "_ROTATE"))
(defun c:S () (command "_SAVE"))
(defun c:SC () (command "_SCALE"))
(defun c:SD () (command "_SOLID" "_R" pause pause "" ^C^C))
(defun c:ST () (command "_STRETCH"))
(defun c:U () (command "_UNDO"))
(defun c:US () (command "_UCS" "_S"))
(defun c:V () (command "_VIEW"))
(defun c:VP () (command "_VPOINT"))
(defun c:W () (command "_UCS" "_W"))
(defun c:WB () (command "_WBLOCK"))
(defun c:WP () (command "_UCS" "_W") (command "_PLAN" ""))
(defun c:WS () (command "_-VIEW" "_S"))
(defun c:X () (command "_EXPLODE" (ssget)))
(defun c:XO () (command "_UCS" "_E"))
(defun c:Y () (command "_UCS" "_O"))
(defun c:Z () (command "_ZOOM"))

;; (defun c:DH () (command "_DIM" "_HOR" pause pause pause "" ^C^C))
;; (defun c:DV () (command "_DIM" "_VE" pause pause pause "" ^C^C))
;; (defun c:DA () (command "_DIM" "_AL" pause pause pause "" ^C^C))

; copy objects and change to current layer
(defun c:CL (/ grp)
	(setq grp (ssget))
	(command "_COPY" grp "" "0,0,0" "0,0,0")
	(command "_LAYCUR" grp "")
	(command "_DRAWORDER" grp "" "_F")
	(princ)
)

; copy objects with displacement and change to current layer
(defun c:CX (/ grp1 grp2 n)
	(setq grp1 (ssget))
	(setq grp2 (ssadd))
	(setq n 0)
	(repeat (sslength grp1)
		(command "_COPY" (ssname grp1 n) "" "0,0,0" "0,0,0")
		(setq grp2 (ssadd (entlast) grp2))
		(setq n (+ n 1))
	)
	(command "_LAYCUR" grp2 "")
	(command "_MOVE" grp2 "")
	(princ)
)

; reference rotate (from center rotation)
(defun c:RR (/ grp cnt ref)
	(prompt "\nSelect entities to rotate: ")
	(setq grp (ssget))
	(setq cnt (getpoint "Rotation point: "))
	(setq ref (getpoint "Reference point: "))
	(command "_ROTATE" grp "" cnt "_R" cnt ref)
)


; copy and rotate objects
(defun c:CR (/ grp1 grp2 cnt ref n)
	(command "_UNDO" "_BEGIN")
	(prompt "\nSelect entities to copy and rotate: ")
	(setq grp1 (ssget))
	(setq cnt (getpoint "Rotation point: "))
	(setq ref (getpoint cnt "Reference point: "))
	(while t
		(setq grp2 (ssadd))
		(setq n 0)
		(repeat (sslength grp1)
			(command "_COPY" (ssname grp1 n) "" "0,0,0" "0,0,0")
			(setq grp2 (ssadd (entlast) grp2))
			(setq n (+ n 1))
		)
		(command "_ROTATE" grp2 "" cnt "_R" cnt ref pause)
	)
	(command "_UNDO" "_END")
)


; save view presets
(defun save-view (name-str /) 
	(command "_-VIEW" "_S" name-str))

(defun c:W1 () (save-view "1"))
(defun c:W2 () (save-view "2"))
(defun c:W3 () (save-view "3"))
(defun c:W4 () (save-view "4"))
(defun c:W5 () (save-view "5"))
(defun c:W6 () (save-view "6"))
(defun c:W7 () (save-view "7"))
(defun c:W8 () (save-view "8"))
(defun c:W9 () (save-view "9"))
(defun c:W0 () (save-view "0"))


; restore view presets
(defun restore-view (name-str /) 
	(command "_-VIEW" "_R" name-str))

(defun c:1 () (restore-view "1"))
(defun c:2 () (restore-view "2"))
(defun c:3 () (restore-view "3"))
(defun c:4 () (restore-view "4"))
(defun c:5 () (restore-view "5"))
(defun c:6 () (restore-view "6"))
(defun c:7 () (restore-view "7"))
(defun c:8 () (restore-view "8"))
(defun c:9 () (restore-view "9"))
(defun c:0 () (restore-view "0"))


; save ucs presets
(defun save-ucs (name-str /) 
	(command "_UCS" "_S" name-str))

(defun c:U1 () (save-ucs "1"))
(defun c:U2 () (save-ucs "2"))
(defun c:U3 () (save-ucs "3"))
(defun c:U4 () (save-ucs "4"))
(defun c:U5 () (save-ucs "5"))
(defun c:U6 () (save-ucs "6"))
(defun c:U7 () (save-ucs "7"))
(defun c:U8 () (save-ucs "8"))
(defun c:U9 () (save-ucs "9"))
(defun c:U0 () (save-ucs "0"))


; restore ucs presets
(defun restore-ucs (name-str /) 
	(command "_UCS" "_R" name-str))

(defun c:11 () (restore-ucs "1"))
(defun c:22 () (restore-ucs "2"))
(defun c:33 () (restore-ucs "3"))
(defun c:44 () (restore-ucs "4"))
(defun c:55 () (restore-ucs "5"))
(defun c:66 () (restore-ucs "6"))
(defun c:77 () (restore-ucs "7"))
(defun c:88 () (restore-ucs "8"))
(defun c:99 () (restore-ucs "9"))
(defun c:00 () (restore-ucs "0"))


; hatch macros
(defun user-hatch (ang dist /) 
	(command "_-BHATCH" "P" "_USER" ang dist "_N"	"_A" "_A" "_N" ""))

(defun c:HE () (command "_-HATCHEDIT" pause "" "" "" "" "" ^c^c))
(defun c:H25 () (user-hatch 45 0.025))
(defun c:H5 () (user-hatch 45 0.05))
(defun c:H10() (user-hatch 45 0.1))
(defun c:P10() (user-hatch 0 0.1))
(defun c:HS () (command "_-BHATCH" "_P" "_SOLID" "_A" "_A" "_N" ""))







;
(prompt "macros.lsp successfully loaded!\n")
