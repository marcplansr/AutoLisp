; backup.lsp
; 181213
; saves dwg backup with current date
 
(defun c:SV (/ pre pre_bak nom org len m n crt nam dat lin)
  (setq pre (getvar "dwgprefix"))
  (setq pre_bak (strcat pre "bak/"))
  (setq nom (getvar "dwgname"))
  (setq org (strcat pre nom))
  (setq len (- (strlen nom) 4))
  (setq m 1)
  (while (/= m len)
    (setq n (+ m 1))
    (setq crt (substr nom m 1))
    (setq m (+ m 1))
    (if	(= crt "#")
      (progn (setq n (- m 2)) (setq m len))
    )
  )
  (setq nam (substr nom 1 n))
  (setq dat (menucmd "m=$(edtime,$(getvar,date),yymoddhhmmss)"))
  (setq lin (strcat pre_bak nam "#" dat))
  (setvar "filedia" 0)
  (command "saveas" "2007" lin)
  (command "saveas" "2007" org "_y")
  (setvar "filedia" 1)
  (terpri)(princ)
)

;
(prompt "backup.lsp successfully loaded!\n")

