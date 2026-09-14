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

;;; event dispatching
(defclass elemental (tui)
  ((%root-view :initarg :root-view
               :initform nil
               :reader root-view
               :type (or null view))))

(defgeneric render (ui)
  (:documentation "should return the root of view tree"))

(defmethod redisplay ((tui elemental))
  (setf (slot-value tui '%root-view) (render tui)))

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
(declaim (inline rect-start rect-size rect-cross-start rect-cross-size
                 copy-rect-along make-rect-along))
(defun rect-start (rect axis)
  (if (eq axis :horizontal) (rect-x rect) (rect-y rect)))
(defun rect-size (rect axis)
  (if (eq axis :horizontal) (rect-cols rect) (rect-rows rect)))
(defun rect-cross-start (rect axis)
  (if (eq axis :horizontal) (rect-y rect) (rect-x rect)))
(defun rect-cross-size (rect axis)
  (if (eq axis :horizontal) (rect-rows rect) (rect-cols rect)))

(defun copy-rect-along (rect axis &key start size cross-start cross-size)
  (if (eq axis :horizontal)
      (copy-rect rect :x start :cols size :y cross-start :rows cross-size)
      (copy-rect rect :y start :rows size :x cross-start :cols cross-size)))

(defun make-rect-along (axis &key start size cross-start cross-size)
  (if (eq axis :horizontal)
      (make-rect :x start :cols size :y cross-start :rows cross-size)
      (make-rect :y start :rows size :x cross-start :cols cross-size)))

(defun backwards-blit (src dest axis)
  "Copies the cells of `src' to `dest', which lies further along `axis'.
Do it furthest first to avoid erasing content."
  (flet ((index (along cross)
           (if (eq axis :horizontal)
               (array-row-major-index *put-buffer* cross along)
               (array-row-major-index *put-buffer* along cross))))
    (loop
      :for offset :downfrom (1- (rect-size src axis)) :to 0
      :do (loop
            :for cross :from (rect-cross-start src axis)
              :below (+ (rect-cross-start src axis) (rect-cross-size src axis))
            ;; dest.start+src.size <= dest.start+dest.size <= bound
            :do (setf (row-major-aref *put-buffer*
                                      (index (+ (rect-start dest axis) offset)
                                             cross))
                      (copy-cell (row-major-aref *put-buffer*
                                                 (index (+ (rect-start src axis) offset)
                                                        cross))))))))

(defstruct (container-cursor (:conc-name cursor-))
  axis
  rect
  child-rect
  limit
  (max-cross-size 0)
  (children (list))
  (child-bgs (list))
  (growth-factors (list)))

(defun setup-container (rect axis)
  "Validates `rect' and returns a cursor placing children along `axis'."
  (check-type axis (member :horizontal :vertical))
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
  (setf rect (clamp-rect rect (screen-rect)))
  (make-container-cursor :axis axis
                         :rect rect
                         :child-rect rect
                         :limit (+ (rect-start rect axis) (rect-size rect axis))))

(declaim (inline remaining-rect))
(defun remaining-rect (cursor)
  "The space left for the cursor's next child."
  (cursor-child-rect cursor))

(declaim (inline container-full-p))
(defun container-full-p (cursor)
  "Is there no space left for another child?"
  (let ((r (cursor-child-rect cursor)))
    (or (zerop (rect-cols r)) (zerop (rect-rows r)))))

(defun place-child (cursor view &optional grow fill-bg)
  "Records a rendered `view' to the cursor as is and advances to its boundary.
Returns `view', ignoring NIL."
  (when view
    (let ((parent (cursor-rect cursor))
          (axis (cursor-axis cursor)))
      (setf (rect view) (clamp-rect (rect view) parent))
      (alexandria:maxf (cursor-max-cross-size cursor) (rect-cross-size (rect view) axis))
      (setf (cursor-child-rect cursor)
            (clamp-rect (copy-rect-along (cursor-child-rect cursor) axis
                                         :start (+ (rect-start (rect view) axis)
                                                   (rect-size (rect view) axis)))
                        parent))
      (push (or grow 0) (cursor-growth-factors cursor))
      (push fill-bg (cursor-child-bgs cursor))
      (push view (cursor-children cursor))))
  view)

(defun pad-cells (cursor n)
  "Leaves `n' cells free along the cursor's axis."
  (unless (container-full-p cursor)
    (place-child cursor
                 (make-instance 'view
                                :rect (copy-rect-along (cursor-child-rect cursor)
                                                       (cursor-axis cursor)
                                                       :size n :cross-size 0)))))

(defun layout-container (cursor)
  "Distributes the free space among children by growth factor and returns the
container view with its children in the order they were placed."
  (let* ((axis (cursor-axis cursor))
         (parent (cursor-rect cursor))
         (child-rect (cursor-child-rect cursor))
         (limit (cursor-limit cursor))
         (children (cursor-children cursor))
         (growth-factors (cursor-growth-factors cursor))
         (expansions (list))
         noalloc)
    ;; growth-factors to cells, first gets rest
    (let ((free (- limit (rect-start child-rect axis))) ; clamped, >= 0
          (total-factor (reduce #'+ growth-factors)))
      ;; we can be space conservative if nobody wants to expand
      (if (= 0 total-factor)
          (setf noalloc t)
          (loop :for allocated = 0 :then (+ allocated allocation)
                :for w :in growth-factors
                :for allocation = (if (zerop w)
                                      0 ; assuming factors >= 0, this is non-negative
                                      (truncate free (/ total-factor w)))
                :do (push allocation expansions)
                :finally (incf (car expansions) (- free allocated))
                         (setf expansions (nreverse expansions)))))
    ;; allocate expansion space backwards, while shifting cells forwards
    ;; using fill-rect to fill in the gaps according to child-bgs
    (loop :for view :in children
          :for expansion = (or (pop expansions) (loop-finish))
          :for fill-bg :in (cursor-child-bgs cursor)
          :for old = (rect view)
          :for old-size = (rect-size old axis)
          :for new-size = (+ old-size expansion)
          :for start = (- limit new-size) :then (- start new-size)
          :for delta = (- start (rect-start old axis))
          :do (setf (rect view) (copy-rect-along old axis :start start :size new-size))
              (unless (zerop delta)
                (dolist (child (children view))
                  (view-traverse child (lambda (v)
                                         (setf (rect v)
                                               (copy-rect-along
                                                (rect v) axis
                                                :start (+ delta
                                                          (rect-start (rect v) axis))))
                                         t))))
              (backwards-blit old (rect view) axis)
              (fill-rect (make-style :bg fill-bg)
                         (make-rect-along axis :start old-size :size expansion
                                               :cross-start 0
                                               :cross-size (rect-cross-size (rect view) axis))
                         (rect view) :char #\space))
    ;; take as little space as possible
    (make-instance 'view :rect (copy-rect-along parent axis
                                                :size (when noalloc
                                                        (- (rect-start child-rect axis)
                                                           (rect-start parent axis)))
                                                :cross-size (cursor-max-cross-size cursor))
                         :focused (some #'focused children)
                         :children (nreverse children))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun place-expansion (cursor var form)
    (with-gensyms (view grow fill-bg)
      `(unless (container-full-p ,cursor)
         (multiple-value-bind (,view ,grow ,fill-bg)
             (let ((,var (cursor-child-rect ,cursor)))
               (declare (ignorable ,var))
               ,form)
           (place-child ,cursor ,view ,grow ,fill-bg))))))

(defmacro with-container ((rect axis &optional cursor) &body body)
  "Evaluates `body' to place the children of a container along `axis' in order.
The following macro bindings are supplied to record views, ignoring their arguments
once the container is full.
- (place (var) form) evaluates `form' with `var' bound to the remaining space.
`form' should return (values view grow &optional fill-bg). `view''s rect should bound the
area drawn to the buffer, and is clamped to `rect'. A positive `grow' is the *proportion* of
the space left once all children are placed that the view expands into, filled with
`fill-bg'; without growth the container takes as little space as possible.
- (pad n) leaves `n' cells empty.
- (full) is true once the container is full, so the body can stop early.
- `cursor' when provided is bound to the container's cursor for use with the
helpers `place-child', `pad-cells', `container-full-p' and `remaining-rect'.
These must NOT be called within place.
Returns the container view."
  (let ((cursor (or cursor (gensym "CURSOR"))))
    `(let ((,cursor (setup-container ,rect ,axis)))
       (macrolet ((place ((var) form) (place-expansion ',cursor var form))
                  (pad (n) `(pad-cells ,',cursor ,n))
                  (full () `(container-full-p ,',cursor)))
         ,@body)
       (layout-container ,cursor))))

(defmacro with-horizontal ((rect &optional cursor) &body body)
  `(with-container (,rect :horizontal ,cursor) ,@body))

(defmacro with-vertical ((rect &optional cursor) &body body)
  `(with-container (,rect :vertical ,cursor) ,@body))
