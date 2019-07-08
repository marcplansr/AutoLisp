; ombra.lsp
; 161227







;;; calcul angles alfa i beta patrons ombres certificat eficiència energètica sortida text
;;; (el punt on es calcula ombrejament ha de ser 0,0,0 i el nord s'ha d'orientar a 90 graus)
(defun C:ppf ( / txt n pt1 pt2 alfa os beta)
  (setq file (open "Bricsys/BricsCAD/V16x64/en_US/Support/Lisp/patrons_ombres.txt" "w"))
  (write-line "angles patrons ombres:" txt)
  (write-line " " txt)
  (write-line " " txt)
  (close txt)
  (setq n 0)
  (setq pt1 (getpoint '(0 0 0) (strcat "Entreu punt " (rtos (+ n 1) 2 0) ":")))
  (while pt1
    (setq n (+ n 1))
    (setq pt2 (list (car pt1) (cadr pt1) 0))
    (setq alfa (* (angle '(0 0 0) pt2) (/ 180 pi)))
    (if (and (>= alfa 0) (< alfa 90))
      (setq alfa (* (+ alfa 90) -1))
      (setq alfa (- 270 alfa))
    )
    (setq pt1 (trans pt1 1 0))
    (setq os (getvar "osmode"))
    (setvar "osmode" 0)
    (command "_ucs" "3" "0,0,0" pt2 "0,0,1")
    (setq beta (* (angle '(0 0 0) (trans pt1 0 1)) (/ 180 pi)))
    (command "_ucs" "_p")
    (setvar "osmode" os)
    (setq txt (open "Bricsys/BricsCAD/V16x64/en_US/Support/Lisp/patrons_ombres.txt" "a"))
    (write-line (strcat "alfa p" (rtos n 2 0)"= " (rtos alfa 2 2) "    beta p" (rtos n 2 0)"= " (rtos beta 2 2)) txt)
    (write-line " " txt)
    (close txt)
    (setq pt1 (getpoint '(0 0 0) (strcat "Entreu punt " (rtos (+ n 1) 2 0) ":")))
    (terpri)
  )
)
;;; idem anterior pero definint arxiu
(defun C:pp ( / nom str_px str_py str_pz arxiu txt n pt1 pt2 alfa os beta)
  (setq nom (getstring "Entra referència del patró d'ombres: "))
  (if (= nom "") (setq nom "ombra"))
  (setq arxiu (strcat "Bricsys/BricsCAD/V16x64/en_US/Support/Lisp/" nom ".txt"))  
  (setq txt (open arxiu "w"))
  (write-line nom txt)
  (write-line "Angles patrons ombres:" txt)
  (write-line " " txt)
  (write-line " " txt)
  (close txt)
  (setq n 0)
  (setq pt1 (getpoint '(0 0 0) (strcat "Entreu punt " (rtos (+ n 1) 2 0) ":")))
  (while pt1
    (if (< (car pt1) 0) 
      (setq str_px (rtos (car pt1) 2 4))
      (setq str_px (strcat " " (rtos (car pt1) 2 4)))
    )
    (if (< (cadr pt1) 0) 
      (setq str_py (rtos (cadr pt1) 2 4))
      (setq str_py (strcat " " (rtos (cadr pt1) 2 4)))
    )
    (if (< (caddr pt1) 0) 
      (setq str_pz (rtos (caddr pt1) 2 4))
      (setq str_pz (strcat " " (rtos (caddr pt1) 2 4)))
    )
    (setq n (+ n 1))
    (setq pt2 (list (car pt1) (cadr pt1) 0))
    (setq alfa (* (angle '(0 0 0) pt2) (/ 180 pi)))
    (if (and (>= alfa 0) (< alfa 90))
      (setq alfa (* (+ alfa 90) -1))
      (setq alfa (- 270 alfa))
    )
    (setq pt1 (trans pt1 1 0))
    (setq os (getvar "osmode"))
    (setvar "osmode" 0)
    (command "_ucs" "3" "0,0,0" pt2 "0,0,1")
    (setq beta (* (angle '(0 0 0) (trans pt1 0 1)) (/ 180 pi)))
    (command "_ucs" "_p")
    (setvar "osmode" os)
    (setq txt (open arxiu "a"))
    (write-line (strcat "X" (rtos n 2 0)"= " str_px) txt)
    (write-line (strcat "Y" (rtos n 2 0)"= " str_py) txt)
    (write-line (strcat "Z" (rtos n 2 0)"= " str_pz) txt)
    (write-line (strcat "alfa p" (rtos n 2 0)"= " (rtos alfa 2 2)) txt)
    (write-line (strcat "beta p" (rtos n 2 0)"= " (rtos beta 2 2)) txt)
    (write-line " " txt)
    (close txt)
    (setq pt1 (getpoint '(0 0 0) (strcat "Entreu punt " (rtos (+ n 1) 2 0) ":")))
    (terpri)
  )
)

;
(prompt "ombra.lsp successfully loaded!\n")




;;;
;;; calcul angles alfa i beta patrons ombres certificat eficiència energètica
;;; (el punt on es calcula ombrejament ha de ser 0,0,0 i el nord s'ha d'orientar a 90 graus)
;;;(defun C:po ( / pt1 pt2 alfa os beta)
;;;  (setq pt1 (getpoint '(0 0 0) "Entreu punt: "))
;;;  (setq pt2 (list (car pt1) (cadr pt1) 0))
;;;  (setq alfa (* (angle '(0 0 0) pt2) (/ 180 pi)))
;;;  (if (and (>= alfa 0) (< alfa 90))
;;;    (setq alfa (* (+ alfa 90) -1))
;;;    (setq alfa (- 270 alfa))
;;;  )
;;;  (setq pt1 (trans pt1 1 0))
;;;  (setq os (getvar "osmode"))
;;;  (setvar "osmode" 0)
;;;  (command "_ucs" "3" "0,0,0" pt2 "0,0,1")
;;;  (setq beta (* (angle '(0 0 0) (trans pt1 0 1)) (/ 180 pi)))
;;;  (command "_ucs" "_p")
;;;  (setvar "osmode" os)
;;;  (terpri)
;;;  (prompt (strcat "alfa = " (rtos alfa 2 2)))
;;;  (terpri)
;;;  (prompt (strcat "beta = " (rtos beta 2 2)))
;;;  (princ)
;;;)
;;;
;;;
;;;
;;;
;;;
;;;