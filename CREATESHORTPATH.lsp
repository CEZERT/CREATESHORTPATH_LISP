(defun c:CreateShortPath (/ GetMidPoints SortPointList CSP_Point CSP_Selection CSP_PointList)

   (defun GetMidPoints (GMP_Selection / GMP_Object GMP_Point1 GMP_Point2 GMP_Return)
      (if
         (and
            (= (type GMP_Selection) 'PICKSET)
            (> (sslength GMP_Selection) 0)
         )
         (progn
            (foreach GMP_Object (vl-remove-if '(lambda (GMP_Item) (listp (cadr GMP_Item))) (ssnamex GMP_Selection))
               (if
                  (vlax-method-applicable-p (setq GMP_Object (vlax-ename->vla-object (cadr GMP_Object))) 'GetBoundingBox)
                  (progn
                     (vla-GetBoundingBox GMP_Object 'GMP_Point1 'GMP_Point2)
                     (setq GMP_Return
                        (cons
                           (mapcar '(lambda (GMP_Value1 GMP_Value2) (/ (+ GMP_Value1 GMP_Value2) 2.0)) (vlax-safearray->list GMP_Point1) (vlax-safearray->list GMP_Point2))
                           GMP_Return                        
                        )
                     )
                  )
               )
            )
         )
      )
      GMP_Return
   )

   (defun SortPointList (SPL_PointList / SPL_Point SPL_Return)
      (setq SPL_PointList (vl-sort SPL_PointList '(lambda (SPL_Item1 CSPCSP_Item2)(< (distance SPL_Item1 (car SPL_PointList))(distance CSPCSP_Item2 (car SPL_PointList))))))
      (repeat (length SPL_PointList)
         (setq SPL_Return (cons (setq SPL_Point (car SPL_PointList)) SPL_Return))
         (setq SPL_PointList (cdr SPL_PointList))
         (setq SPL_PointList (vl-sort SPL_PointList '(lambda (SPL_Item1 CSPCSP_Item2)(< (distance SPL_Item1 SPL_Point)(distance CSPCSP_Item2 SPL_Point)))))
      )
      (reverse SPL_Return)
   )
   
   (if
      (and
         (setq CSP_Selection (ssget))
         (setq CSP_Point (getpoint "\nIndicate startpoint: "))                  
      )
      (progn         
         (setq CSP_PointList (SortPointList (cons CSP_Point (GetMidPoints CSP_Selection))))
         (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
         (entmake
            (append
               (list
                  '(0 . "LWPOLYLINE")
                  '(100 . "AcDbEntity")
                  '(100 . "AcDbPolyline")
                  (cons 90 (length CSP_PointList))
                  (cons 70 (if (= 1 (getvar 'PLINEGEN)) 128 0))
                  '(38 . 0.0)
               )
               (mapcar '(lambda (CSP_Item) (cons 10 CSP_Item)) CSP_PointList)
               (list '(210 0.0 0.0 1.0))
            )
         )         
         (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
      )                
   )
   (princ) 
)  