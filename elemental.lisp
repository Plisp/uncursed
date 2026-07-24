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
   (%children :initarg :children
              :initform (list)
              :accessor children
              :type list)
   (%focused :initarg :focused
             :initform nil
             :accessor focused
             :type boolean)
   (%key-handler :initarg :key-handler
                 :initform (constantly nil)
                 :accessor key-handler
                 :type function)
   (%mouse-handler :initarg :mouse-handler
                   :initform (constantly nil)
                   :accessor mouse-handler
                   :type function)))

(defmethod print-object ((elt view) stream)
  (print-unreadable-object (elt stream :identity t :type t)
    (when (rect elt)
      (format stream "y:~a x:~a" (rect-y (rect elt)) (rect-x (rect elt))))))

(defgeneric render (thing rect)
  (:documentation "(thing,rect) -> (values view grow &optional fill-bg)"))

;;; event dispatching
(defclass elemental (tui)
  ((%root-view :initarg :root-view
               :initform nil
               :reader root-view
               :type (or null view))))

(defmethod redisplay ((tui elemental))
  (setf (slot-value tui '%root-view)
        (render tui (make-rect :x 0 :y 0 :cols (cols tui) :rows (rows tui)))))

(defun view-traverse (view callback)
  (let ((res (funcall callback view)))
    (when res
      (some (alexandria:rcurry #'view-traverse callback) (children view)))))

(defmethod dispatch-event ((tui elemental) event)
  (if (mouse-event-p event)
      ;; mouse events are globally visible, necessary for patterns like click and hold
      (view-traverse (root-view tui)
                     (lambda (view)
                       (funcall (mouse-handler view) view event)
                       t))
      (view-traverse (root-view tui)
                     (lambda (view)
                       (when (focused view)
                         (funcall (key-handler view) view event))
                       t))))

;;
;;; containers
;;

;; currently if two alike containers are spread along the same axis, the first one
;; will be allowed to allocate all the space.
;; Shrinking may trigger arbitrary reallocations of children which significantly
;; complicates the algorithm and may affect performance. In this case perhaps the user
;; should be responsible for making global decisions to rebalance things?
(flet
    ((render-container (rect renderables
                        coord span other-span blitter copier copier2 make-fillrect)
       (let ((children (list))
             (child-bgs (list))
             (child-rect rect)
             (rect-x2 (+ (funcall coord rect) (funcall span rect)))
             (max-height 0)
             (growth-factors (list))
             (expansions (list))
             noalloc)
         (or (<= (rect-y2 rect) (array-dimension *put-buffer* 0))
             (error 'rect-bounds-error
                    :coordinate (rect-y2 rect)
                    :bounds :line
                    :rect (screen-rect)))
         (or (<= (rect-x2 rect) (array-dimension *put-buffer* 1))
             (error 'rect-bounds-error
                    :coordinate (rect-x2 rect)
                    :bounds :column
                    :rect (screen-rect)))
         (or ())
         (setf rect
               (clamp-rect rect (make-rect :x 0 :y 0
                                           :rows (array-dimension *put-buffer* 0)
                                           :cols (array-dimension *put-buffer* 1))))
         (loop :for thing :in renderables
               :do (multiple-value-bind (view grow fill-bg)
                       (render thing child-rect)
                     (or view (loop-finish))
                     (or grow (setf grow 0))
                     (alexandria:maxf max-height (funcall other-span (rect view)))
                     (setf (rect view) (clamp-rect (rect view) rect))
                     ;; update x for the next invocation
                     (setf child-rect (funcall copier child-rect
                                               (+ (funcall coord (rect view))
                                                  (funcall span (rect view)))
                                               (rect-cols child-rect))
                           child-rect (clamp-rect child-rect rect))
                     (push grow growth-factors)
                     (push fill-bg child-bgs)
                     (push view children)))
         ;; growth-factors to cells, first gets rest
         (let ((free-cols (- rect-x2 (funcall coord child-rect))) ; clamped, >= 0
               (total-factor (reduce #'+ growth-factors)))
           ;; we can be space conservative if nobody wants to expand
           (if (= 0 total-factor)
               (setf noalloc t)
               (loop :for allocated = 0 :then (+ allocated allocation)
                     :for w :in growth-factors
                     :for allocation = (if (zerop w)
                                           0 ; assuming factors >= 0, this is non-negative
                                           (truncate free-cols (/ total-factor w)))
                     :do (push allocation expansions)
                     :finally (incf (car expansions) (- free-cols allocated))
                              (setf expansions (nreverse expansions)))))
         ;; allocate expansion space backwards, while shifting cells forwards
         ;; using fill-rect to fill in the gaps according to child-bgs
         (loop :for view :in children
               :for expansion = (or (pop expansions) (loop-finish))
               :for fill-bg :in child-bgs
               :for old = (rect view)
               :for old-cols = (funcall span (rect view))
               :for new-cols = (+ old-cols expansion)
               :for end-offset = (- rect-x2 new-cols) :then (- end-offset new-cols)
               :do (view-traverse view (lambda (v)
                                         (setf (rect v)
                                               (funcall copier (rect v) end-offset new-cols))
                                         t))
                   (funcall blitter old (funcall copier old end-offset new-cols))
                   (fill-rect (make-style :bg fill-bg)
                              (funcall make-fillrect ; XXX how 2 permute arguments ?
                                       0 (funcall other-span (rect view))
                                       old-cols expansion)
                              (rect view) #\space))
         ;; take as little space as possible
         (make-instance 'view :rect (funcall copier2 rect
                                             (when noalloc
                                               (- (funcall coord child-rect)
                                                  (funcall coord rect)))
                                             max-height)
                              :focused (some #'focused children)
                              :children children)))
     (row-backwards-blit (src dest)
       (loop
         :for x-offset :downfrom (1- (rect-cols src)) :to 0
         :do (loop
               :for y :from (rect-y src) :below (rect-y2 src)
               ;; dest.x+src.cols <= dest.x+dest.cols <= x bound
               :do (setf (aref *put-buffer* y (+ (rect-x dest) x-offset))
                         (copy-cell (aref *put-buffer* y (+ (rect-x src) x-offset)))))))
     (col-backwards-blit (src dest)
       (loop
         :for y-offset :downfrom (1- (rect-rows src)) :to 0
         :do (loop
               :for x :from (rect-x src) :below (rect-x2 src)
               :do (setf (aref *put-buffer* (+ (rect-y dest) y-offset) x)
                         (copy-cell (aref *put-buffer* (+ (rect-y src) y-offset) x)))))))

  (defun horizontal-container (rect renderables)
    "The `rect' argument indicates the maximum bounds for this container, which may
not be reached unless the last child element has positive grow factor.
`render' takes a (thing,rect) -> (values view grow &optional fill-bg)
`view''s rect should bound the area drawn to the buffer, and is clamped to `rect'.
`grow' is a non-negative integer indicating the *proportion* of free space to expand.
If it is zero, no expansion occurs."
    (render-container rect renderables #'rect-x #'rect-cols #'rect-rows #'row-backwards-blit
                      (lambda (rect a b) (copy-rect rect :x a :cols b))
                      (lambda (rect a b) (copy-rect rect :cols a :rows b))
                      (lambda (a b c d) (make-rect :y a :rows b :x c :cols d))))

  (defun vertical-container (rect renderables)
    "The `rect' argument indicates the maximum bounds for this container, which may
not be reached unless the last child element has positive grow factor.
`view''s rect should bound the area drawn to the buffer, and is clamped to `rect'.
`grow' is a non-negative integer indicating the *proportion* of free space to expand.
If it is zero, no expansion occurs."
    (render-container rect renderables #'rect-y #'rect-rows #'rect-cols #'col-backwards-blit
                      (lambda (rect a b) (copy-rect rect :y a :rows b))
                      (lambda (rect a b) (copy-rect rect :rows a :cols b))
                      (lambda (a b c d) (make-rect :x a :cols b :y c :rows d)))))
