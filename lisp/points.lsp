; points.lsp
; 171216

; div-flag T, divides plines in regular segments
; to match java import using ArrayList data type
(defun c:GG () 
	(command "_UNDO" "_BEGIN")	(setvar "OSMODE" 0)
	(main T) (command "_UNDO" "_END")	(terpri))

; div-flag nil, divides plines to match vertices
; to match java import using ArrayList data type
(defun c:FF () 
	(command "_UNDO" "_BEGIN")	(setvar "OSMODE" 0)
	(main nil) (command "_UNDO" "_END") (terpri))



(defun main (div-flag / grp-all grp-points grp-polylines
		grp-lwpolylines list-points list-lwpolylines 
		list-polylines ent-id vert-id file-name file list-sizes)
	
	(setq grp-all (select-objects)) ; select objects func
	(init-vars) ; init void vars func
	(sort-entities grp-all) ; sorts entities by type to each group
	
	; lists processing
	(if (not (null grp-points))
		(setq list-points (process-points grp-points)))
	(if (not (null grp-lwpolylines))
		(setq list-lwpolylines
			(process-polylines grp-lwpolylines div-flag)))
	(if (not (null grp-polylines))
		(setq list-polylines
			(process-polylines grp-polylines div-flag)))

	;;; DEBUG
	; (if (not (null list-lwpolylines))
	; 	(draw-pl-list list-lwpolylines))
	; (if (not (null list-polylines))
	; 	(draw-pl-list list-polylines))
	;;; DEBUG

	; csv output, pontSet.cvs
	(setq file-name "pointSet.csv")
	(setq file-name (strcat 
		"/home/x/workspaceJava/Delaunay/data/" file-name))
	(setq file (open file-name "w"))
	(if (not (null list-points))
		(write-list list-points))
		(setq ent-id 0)
	(if (not (null list-lwpolylines))
		(write-pl-list list-lwpolylines))
	(if (not (null list-polylines))
		(write-pl-list list-polylines))
	(close file)
)


(defun point-gen (point-num x-low x-high y-low y-high decimal-places / 
		point-list new-point max-dist search smaller point)
	(setq point-list (list))
	(setq max-dist 
		(/ (min (- x-high x-low) (- y-high y-low)) (sqrt point-num)))
	(repeat point-num
		(setq search T)
		(while search
			(setq new-point 
				(create-point x-low x-high y-low y-high decimal-places))
			(setq search nil)
			(if (> (length point-list) 0)
				(foreach point point-list
					(if (< (points-dist new-point point) max-dist)
						(setq search T)
					)
				)
			)
		)
		(setq point-list (append point-list (list new-point)))
	)
	(foreach point point-list
		(command "_point" point))
	(print max-dist)
	(print point-list)
	(command "_circle" "400,-300" (/ max-dist 2))
)


(defun create-point (x-low x-high y-low y-high decimal-places / 
		x-coord y-coord)
	(setq x-coord 
		(randrange x-low x-high decimal-places))
	(setq y-coord 
		(randrange y-low y-high decimal-places))
	(list x-coord y-coord)
)


; init void vars
(defun init-vars ()
	(setq grp-points (ssadd))
	(setq grp-polylines (ssadd))
	(setq grp-lwpolylines (ssadd))
	(setq list-points (list))
	(setq list-lwpolylines (list))
	(setq list-polylines (list))
	; (setq ent-id -1)
	(setq vert-id nil)
)


; sorts given group assigning each entity
; to correspondent type group, depending
; if it is a point, a lwpolyline or a polyline
(defun sort-entities (grp-all / ent-index 
		nent lent)
	(setq ent-index 0)
	(repeat (sslength grp-all)
		(setq nent (ssname grp-all ent-index))
		(setq lent (entget nent))
		(if (= (cdr (assoc 0 lent)) "POINT")
			(setq grp-points (ssadd nent grp-points))
		)
		(if (= (cdr (assoc 0 lent)) "POLYLINE")
			(setq grp-polylines (ssadd nent grp-polylines))
		)
		(if (= (cdr (assoc 0 lent)) "LWPOLYLINE")
			(setq grp-lwpolylines (ssadd nent grp-lwpolylines))
		)
		(setq ent-index (+ ent-index 1))
	)
)


; returns a list of point coords
; of a given group of selected points
(defun process-points (grp-points / output
		pnt-index pnt-name pnt-list)
	(setq output (list))
	(setq pnt-index 0)
	(repeat (sslength grp-points)
		(setq pnt-name (ssname grp-points pnt-index))
		(setq pnt-list (entget pnt-name))
		(setq output (append output 
			(list (cdr (assoc 10 pnt-list)))))
		(setq pnt-index (+ pnt-index 1))
	)
	(princ output)
)


; returns a list of point coords from aplying
; functions that divide lwpolylines in given group
(defun process-polylines (grp-polylines  div-flag / output
		index polyline-name polyline-list 
		polyline-verts div-list)
	(setq output (list))
	(setq index 0)
	(repeat (sslength grp-polylines)
		(setq polyline-name (ssname grp-polylines index))
		(setq polyline-list (entget polyline-name))
		(if (= (cdr (assoc 0 polyline-list)) "LWPOLYLINE")
			(setq polyline-verts 
				(get-lwpolyline-verts polyline-list))
			(setq polyline-verts 
				(get-polyline-verts polyline-name))
		)
		(setq div-list (merge-pl-points 
			polyline-name polyline-verts div-flag))
		(setq output (append output (list div-list)))
		(setq index (+ index 1))
	)
	(princ output)
)


; merges point list resulting from pl division
; and first and last points of given polyline
; applies to both types, polyline and lwpolyline
(defun merge-pl-points (polyline verts-list div-flag / 
		output first-point last-point mid-points)
	(if div-flag
		(progn
			(setq output (list))
			(setq first-point (first-in-list verts-list))
			(setq last-point (last-in-list verts-list))
			(setq mid-points (divide-pl polyline))
			(setq output (append output 
				(list first-point) mid-points (list last-point)))
		)
		(setq output (alt-divide verts-list))
	)
	(princ output)
)


; takes a LWPOLYLINE type entity
; returns list of all vertices, first to last
(defun get-lwpolyline-verts (lwpolyline-list / output
		z-coord element index coord-list)
	(setq output (list))
	(setq z-coord (cdr (assoc 38 lwpolyline-list)))
	(foreach element lwpolyline-list
		(progn
			(setq index (car element))
			(if (= index 10)
				(progn
					(setq coord-list (list))
					(setq coord-list (append coord-list 
						(cdr element) (list z-coord)))
					(setq output (append output (list coord-list)))
				)
			)
		)
	)
	(princ output)
)


; takes a POLYLINE type entity
; returns list of all vertices, first to last
(defun get-polyline-verts (polyline-name / output
		polyline-list)
	(setq output (list))
	(setq flag T)
	(while flag
		(setq polyline-name (entnext polyline-name))
		(setq polyline-list (entget polyline-name))
		(if (= (cdr (assoc 0 polyline-list)) "VERTEX")
			(setq output (append output 
				(list (cdr (assoc 10 polyline-list)))))
		)
		(if (= (cdr (assoc 0 polyline-list)) "SEQEND")
			(setq flag nil)
		)
	)
	(princ output)
)


; takes a list of point coordinates
; returns first point coords in list
(defun first-in-list (coord-list / output)
	(setq output (nth 0 coord-list))
)


; takes a list of point coordinates
; returns last point coords in list
(defun last-in-list (coord-list / output
		list-length last-index)
	(setq list-length (length coord-list))
	(setq last-index (- list-length 1))
	(setq output (nth last-index coord-list))
)


; returns a list of points resulting from
; the division of the given polyline
; either lwpolyline or polyline type
; distance between points along plyline is
; aproximately global var *gap-dist*
; output does not include first and 
; last points of polyline
(defun divide-pl (polyline / output
		polyline-length segments point-name
		point-list)
	(setq output (list))
	(command "_LENGTHEN" polyline "")
	(setq polyline-length (getvar "PERIMETER"))
	(setq segments (/ polyline-length *pnt-dist*))
	(setq segments (fix segments))
	(command "_DIVIDE" polyline segments)
	(repeat (- segments 1)
		(setq point-name (entlast))
		(setq point-list (entget point-name))
		(setq output (append output 
			(list (cdr (assoc 10 point-list)))))
		(entdel point-name)
	)
	(setq output (reverse output))
)


; divides polyline in segments so that
; resulting points match vertices
(defun alt-divide (verts-list / list-length index
		point1 point2 point-dist segments new-point
		point-angle point-index)
	(setq output (list (nth 0 verts-list)))
	(setq list-length (length verts-list))
	(setq index 0)
	(repeat (- list-length 1)
		(setq point1 (nth index verts-list))
		(setq point2 (nth (+ index 1) verts-list))
		(setq point-dist (distance point1 point2))
		(setq segments (num-segments point-dist))
		(if (> segments 1)
			(progn
				(setq point-dist (/ point-dist segments))
				(setq point-angle (angle point1 point2))
				(setq point-index 1)
				(repeat (- segments 1)
					(setq new-point (polar point1 
						point-angle (* point-dist point-index)))
					(setq output (append output (list new-point)))
					(setq point-index (+ point-index 1))
				)
			)
		)
		(setq output (append output (list point2)))
		(setq index (+ index 1))
	)
	(princ output)
)


; writes point list to cvs file
(defun write-list (list-points / point
		point-str)
	(foreach point list-points
		(progn
			(setq point-str (strcat
				(format-str (nth 0 point)) ", "
				(format-str (nth 1 point)) ", "
				(format-str (nth 2 point)))
			)
			(write-line point-str file)
		)
	)
	
)


; writes all nested point lists in pl list in to cvs file
(defun write-pl-list (list-polylines / index)
	(setq index 0)
	(repeat (length list-polylines)
		(setq vert-id 0)
		(write-line " " file)
		(write-list (nth index list-polylines))
		(setq ent-id (+ ent-id 1))
		(setq index (+ index 1))
	)
	(setq vert-id nil)
)


; takes a number and returns a string that represents that number
; formatted with leading spaces so that nums list looks aligned
(defun format-str (num / num-str len-str prefix)
	(setq num-str (rtos num 2 2))
	(setq len-str (strlen num-str))
	(setq prefix "")
	(if (= len-str 7) (setq prefix " "))
	(if (= len-str 6) (setq prefix "  "))
	(if (= len-str 5) (setq prefix "   "))
	(if (= len-str 4)	(setq prefix "    "))
   (strcat prefix num-str)
)


; takes a number and returns a string that represents that number
; formatted with leading spaces so that nums list looks aligned
(defun format-id (num / num-str len-str prefix)
	(setq num-str (rtos num 2 0))
	(setq len-str (strlen num-str))
	(setq prefix "")
	(if (= len-str 4) (setq prefix " "))
	(if (= len-str 3) (setq prefix "  "))
	(if (= len-str 2) (setq prefix "   "))
	(if (= len-str 1)	(setq prefix "    "))
   (strcat prefix num-str)
)


; draws a point for each coords list in point list
(defun draw-points (list-points / point
		point-str)
	(foreach point list-points
		(progn
			(setq point-str (strcat 
				(rtos (nth 0 point) 2 4) "," 
				(rtos (nth 1 point) 2 4) ","	
				(rtos (nth 2 point) 2 4)))
			(command "_POINT" point-str)
		)
	)
)


; draws all nested point lists in pl list 
(defun draw-pl-list (list-polylines / index)
	(setq index 0)
	(repeat (length list-polylines)
		(draw-points (nth index list-polylines))
		(setq index (+ index 1))
	)
)


; takes a length and divides it in segments so that
; their dimension is the closest to *pnt-dis* global var
(defun num-segments (total-length / segments
		rem1 rem2)
	(setq segments (fix 
		(/ total-length *pnt-dist*)))
	(if (= segments 0) (setq segments 1))
	(setq rem1 (abs 
		(- (/ total-length segments) *pnt-dist*)))
	(setq rem2 (abs 
		(- (/ total-length (+ segments 1)) *pnt-dist*)))
	(if (<= rem2 rem1)
		(setq segments (+ segments 1))
	)
	(princ segments)
)


(prompt "points.lsp loaded successfully!!!\n")
