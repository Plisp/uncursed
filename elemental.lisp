(in-package #:uncursed)

;;
;;; view interface
;;
;; a view describes
;; 1. how to graphically represent an part of the application state
;; 2. how to respond to events
;;

(defclass view ()
  ((%rect :initarg :rect
          :initform (error "view without rect")
          :accessor rect
          :type rect)
   (key-handler :initarg :key-handler
                :initform (constantly nil)
                :accessor key-handler)
   (mouse-handler :initarg :mouse-handler
                  :initform (constantly nil)
                  :accessor mouse-handler)))

(defmethod print-object ((elt view) stream)
  (print-unreadable-object (elt stream :identity t :type t)
    (when (rect elt)
      (format stream "y:~a x:~a" (rect-y (rect elt)) (rect-x (rect elt))))))

(defgeneric render (thing rect))

;;; event dispatching
(defgeneric view-tree (elemental))

(defclass elemental (tui)
  ((%root-view :initarg :root-view
               :reader root-view
               :type view)))

(defmethod view-tree ((tui elemental))
  (error "draw and generate a view tree"))

(defmethod redisplay ((tui elemental))
  (setf (slot-value tui '%root-view) (view-tree tui)))

(defmethod dispatch-event ((tui elemental) event)
  (error "TODO traverse the view tree and dispatch, child chooses whether to propagate"))

;;
;;; containers
;;

;; TODO borders
(defclass container (view)
  ((%children :initarg :children
              :initform (list)
              :accessor children)))

(defun horizontal-container (rect renderables &key key-handler mouse-handler)
  "The `rect' argument indicates the maximum bounds for this container, which may
not be reached unless the last child element is has positive grow factor.
`render' takes a (thing,rect) -> (values view grow &optional fill-bg)
`rect' must not be modified.
`view' must bound the area drawn to the buffer.
`grow' is a non-negative integer indicating the *proportion* of free space to expand.
If it is zero, no expansion occurs."
  (let ((container (make-instance 'container :rect rect
                                             :key-handler key-handler
                                             :mouse-handler mouse-handler))
        (child-bgs (list))
        (child-rect rect)
        (growth-factors (list))
        (expansions (list)))
    (loop
      :for thing :in renderables
      :do (multiple-value-bind (view grow fill-bg)
              (render thing child-rect)
            ;; update x for the next invocation
            (let* ((cols-used (rect-cols (rect view)))
                   (new-x (min (+ (rect-x child-rect) cols-used)
                               (+ (rect-x rect) (rect-cols rect)))))
              (setf child-rect (copy-rect child-rect
                                          :x new-x
                                          :cols (min (- (rect-cols child-rect) cols-used)
                                                     (- (rect-cols rect) new-x))))
              ;; note: x,y not clamped. TODO consider shrinking behaviors
              (setf (rect view)
                    (copy-rect (rect view)
                               :rows (min (rect-rows (rect view)) (rect-rows rect))
                               :cols (min cols-used (- new-x (rect-x (rect view)))))))
            (push grow growth-factors)
            (push fill-bg child-bgs)
            (push view (children container))))
    ;; growth-factors to cells, first gets rest
    (let ((free-cols (- (+ (rect-x rect) (rect-cols rect))
                        (rect-x child-rect)))
          (total-width (reduce #'+ growth-factors)))
      (loop :for allocated = 0 :then (+ allocated allocation)
            :for w :in growth-factors
            :for allocation = (if (zerop w) 0 (truncate free-cols (/ total-width w)))
            :do (push allocation expansions)
            :finally (incf (car expansions) (- free-cols allocated))
                     (setf expansions (nreverse expansions))))
    ;; allocate expansion space backwards, while shifting cells forwards
    ;; using fill-rect to fill in the gaps according to child-bgs
    (flet ((backwards-blit (src dest)
             (loop
               :for x-offset :downfrom (1- (rect-cols src)) :to 0
               :do (loop
                     :for y :from (rect-y src) :below (+ (rect-y src) (rect-rows src))
                     :do (setf (aref *put-buffer* y (+ (rect-x dest) x-offset))
                               (copy-cell
                                (aref *put-buffer* y (+ (rect-x src) x-offset))))))))

      (loop :with end-offset = (+ (rect-x rect) (rect-cols rect))
            :for view :in (children container)
            :for expansion = (pop expansions)
            :for fill-bg :in child-bgs
            :for old = (rect view)
            :for old-cols = (rect-cols (rect view))
            :for new-cols = (+ old-cols expansion)
            :do (decf end-offset new-cols)
                (setf (rect view) (copy-rect old :x end-offset :cols new-cols))
                (backwards-blit old (rect view))
                (fill-rect (make-style :bg fill-bg)
                           (make-rect :y 0 :rows (rect-rows (rect view))
                                      :x old-cols :cols expansion)
                           (rect view) #\space)))
    container))
