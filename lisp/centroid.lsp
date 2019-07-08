; centroid.lsp
; 170914


; get centroid keyboard command call
(defun c:GC (/ lent pcent)
	(setq lent (entget (car (entsel))))
	(setq pcent (get-centroid (get-pl-vertices lent)))
	(command "_POINT" pcent)
)


; function that 
; takes a list of vertices of a given polyline
; returns a point list with centroid coordinates
(defun get-centroid (verts-list / verts-list-len
		area centroid-x centroid-y current-vert-index
		next-vert-index current-x current-y next-x next-y
		output)
	(setq verts-list-len (length verts-list))
	(setq area 0)
	(setq centroid-x 0)
	(setq centroid-y 0)
	(setq current-vert-index 0)
	(repeat verts-list-len
		(setq next-vert-index (+ current-vert-index 1))
		(if (>= next-vert-index verts-list-len)
			(setq next-vert-index 0)
		)

		(setq current-vert (nth current-vert-index verts-list))
		(setq next-vert (nth next-vert-index verts-list))
		
		(setq current-x (nth 0 current-vert))
		(setq current-y (nth 1 current-vert))
		(setq next-x (nth 0 next-vert))
		(setq next-y (nth 1 next-vert))

		(setq area (+ area 
			(nth 0 (calc current-x current-y next-x next-y)))
		)
		(setq centroid-x (+ centroid-x
			(nth 1 (calc current-x current-y next-x next-y)))
		)
		(setq centroid-y (+ centroid-y
			(nth 2 (calc current-x current-y next-x next-y)))
		)

		(setq current-vert-index next-vert-index)
	)
	(setq area (/ area 2))
	(setq centroid-x (/ centroid-x (* 6 area)))
	(setq centroid-y (/ centroid-y (* 6 area)))
	
	(setq output (list))
	(setq output (append output 
		(list centroid-x centroid-y)))
)

; centroid formulas calculation
; takes coords of current and next vertex in pl
; returns a list containing partial calculation of
; area, centroid x coord and centroid y coord
(defun calc (current-x current-y next-x next-y / 
		area centroid-x centroid-y output)
	(setq output (list))
	(setq area 
		(- (* current-x next-y) (* next-x current-y))
	)
	(setq centroid-x 
		(* (+ current-x next-x) area)
	)
	(setq centroid-y 
		(* (+ current-y next-y) area)
	)
	(setq output (append output 
		(list area centroid-x centroid-y)))
)


; aux function that gets x coord of point
; takes a point and returns its x coord
(defun get-x-coord (vertex / output)
	(setq output (nth 0 vertex))
)


; aux function that gets y coord of point
; takes a point and returns its y coord
(defun get-y-coord (vertex / output)
	(setq output (nth 1 vertex))
)


; aux function that gets all vertices in lwpolyline
; takes a lwpolyline entity list (does not check)
; returns a list with all polilyne vertices
(defun get-pl-vertices (pl-list / vertices index)
	(setq vertices (list))
	(foreach element pl-list
		(progn
			(setq index (car element))
			(if (= index 10)
				(setq vertices (append vertices (list (cdr element))))
			)
		)
	)
	(princ vertices)
)


; calculates centroid by mass prop (region)
(defun c:GCC (/ region-ent object object-name)
	(vl-load-com)
	(setq region-ent (entsel "\nSelect a region: "))
	(if region-ent
		(progn
			(setq object (vlax-ename->vla-object (car region-ent)))
			(setq object-name (vla-Get-ObjectName object))
			(if (equal object-name "AcDbRegion")
				(progn
					(setq centroid (vla-Get-Centroid object))
					(setq centroid-list (vlax-safearray->list 
						(vlax-variant-value centroid)))
					; (setq centroid-str (vl-princ-to-string centroid-list))
					(if (< (length centroid-list) 3)
						(setq centroid-list (append centroid-list (list 0.0)))
					)
				)
				(princ "\nRegion was not selected !")
			)
		)
	)
	; (if centroid-list (eval centroid-list))
	(print centroid-list)
	(command "_POINT" centroid-list)
)

(prompt "centroid.lsp successfully loaded!\n")
