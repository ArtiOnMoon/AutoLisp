
(DEFUN c:f_drawobject (/ dcl_id nub A1 A2 A3 B1 B2 B3 C1 D1 R1 R2 R3 Di1 Di2)	
  
  (DEFUN f_draw	(A1 A2 A3 B1 B2 B3 C1 D1 R1 R2 R3 Di1 Di2 / p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 circle pt-on-circle z1 z2)
    
    (setq p1 (getpoint "\n”кажите базовую точку : "))
				
    (setq p2 (polar p1 0 A1))	
    (setq p3 (polar p2 (/ pi 2) B1))
    (setq p4 (polar p3 pi A3))
    (setq p5 (polar (mapcar ' - p2 (list C1 0)) (/ pi 2) (+ B2 B3) ))
    (setq p6 (polar p1 (/ pi 2) R2 ))
    (setq p7 (polar (mapcar ' + p1 (list (- A1 (+ A2 A3)) 0)) (/ pi 2) D1 ))
    (setq p8 (polar p5 (+(/ pi 2) pi) (/ B3 2) ))
    (setq p9 (polar p7 0 (/ A2 2)))
    (setq p10 (polar (mapcar ' - p2 (list A3 0)) (/ pi 2) B2 ))
    (setq p11 (polar p10 (/ pi 2) B3))
    (setq p12 (polar p10 pi A2))

    (setq z1 (polar (mapcar ' - p2 (list 0 50)) pi (+ A1 50 ) ))
    (setq z2 (polar (mapcar ' + p3 (list 0 50)) (/ pi 2) 50 ))
    
    (setq osm (getvar "osmode"))	
    (setvar "osmode" 0)			

    (command "_ZOOM" z1 z2 "")
    (command "_line" p1 p2 "")
    (command "_line" p2 p3 "")
    (command "_line" p3 p4 "")
    (command "_arc" p4 "_e" p5 "_r" R1)
    (command "_line" p1 p6 "")
    (command "_arc" p7 "_e" p6 "_r" R2)
    
    (command "_line" p5 p8 "")
    (command "_line" p7 p9 "")
    (command "_arc" p9 "_e" p8 "_r" R3)
 
    (command "_circle" p10 "_d" Di1)
    (setq circle (entlast))
    (setq pt-on-circle (polar (cdr (assoc 10 (entget circle)))
				   (cdr (assoc 40 (entget circle))) 
                                   (* pi 0.5)))
    (command "_.CENTERMARK" (list circle pt-on-circle) "")
    
    (command "_circle" p11 "_d" Di1)
    
    (setq circle (entlast))
    (setq pt-on-circle (polar (cdr (assoc 10 (entget circle)))
				   (cdr (assoc 40 (entget circle))) 
                                   (* pi 0.5)))
    (setq pt-for-dim (list (+ (car pt-on-circle) (+ A3 20) )
                                  (+ (cadr pt-on-circle) (+ (- B1 (+ B2 B3)) 20 ))
                                  65))
    (command "_.DIMDIAMETER" (list circle pt-on-circle) "_none" pt-for-dim)
    (command "_.CENTERMARK" (list circle pt-on-circle) "")
    
    (command "_circle" p12 "_d" Di2)

    (setq circle (entlast))
    (setq pt-on-circle (polar (cdr (assoc 10 (entget circle)))
				   (cdr (assoc 40 (entget circle))) 
                                   (* pi 0.5)))
    (setq pt-for-dim (list (- (car pt-on-circle)(+ (- A1 (+ A2 A3)) 20 ))
                                  (+ (cadr pt-on-circle) (+ D1 10))
                                  120))
    (command "_.DIMDIAMETER" (list circle pt-on-circle) "_none" pt-for-dim)
    (command "_.CENTERMARK" (list circle pt-on-circle) "")
    
    (command "_dimlinear" p1 p2 "@ 0,-20")
    (command "_dimlinear" p12 p10 (mapcar '+ p1 (list 0 -10)) )
    (command "_dimlinear" p10 p2 "@ 0,-10")
    
    (command "_dimlinear" p2 p3 "@ 20,0")
    (command "_dimlinear" p10 (mapcar ' - p2 (list A3 0)) (mapcar '+ p2 (list 10 0)) )
    (command "_dimlinear" p10 p11 (mapcar '+ p2 (list 10 0)) )

    (command "_dimlinear" p5 (mapcar ' + p5 (list C1 0)) (mapcar ' + p5 (list 0 (+ R1 20)) ) )

    (command "_dimlinear"  p7 (mapcar ' + p1 (list R2 0)) (mapcar '- p1 (list 20 0)) )

    (command "_.dimradius" p5 p4 "")
    (command "_.dimradius" p6 p7 "")
    (command "_.dimradius" p9 p8 "")
    
    (setvar "osmode" osm)	
    
)
    
  (defun getvalue ()
    (setq A1 (atoi (get_tile "key_A1")))
    (setq A2 (atoi (get_tile "key_A2")))
    (setq A3 (atoi (get_tile "key_A3")))
    
    (setq B1 (atoi (get_tile "key_B1")))
    (setq B2 (atoi (get_tile "key_B2")))
    (setq B3 (atoi (get_tile "key_B3"))) 
    (setq C1 (atoi (get_tile "key_C1")))
    (setq D1 (atoi (get_tile "key_D1")))
    (setq R1 (atoi (get_tile "key_R1")))
    (setq R2 (atoi (get_tile "key_R2")))
    (setq R3 (atoi (get_tile "key_R3")))
    (setq Di1 (atoi (get_tile "key_Di1")))
    (setq Di2 (atoi (get_tile "key_Di2")))
  )	  
 
  (setq dcl_id (load_dialog "d:\mp_paral.dcl"))
  (if (not (new_dialog "entervalue" dcl_id))(exit))
  
  (action_tile "accept" "(getvalue) (done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq ddi (start_dialog))
   
  (unload_dialog dcl_id) 

  (if (= ddi 1) (f_draw A1 A2 A3 B1 B2 B3 C1 D1 R1 R2 R3 Di1 Di2))

  (princ)
)
