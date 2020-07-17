(in-package :uncursed)

(defclass text-box (window)
  ((cursor-offset :initform 0
                  :accessor text-box-cursor-offset)
   (text :initarg :text
         :accessor text-box-text))
  (:documentation "Inherits from WINDOW, displays an editable text box."))

(defmethod handle-window-event ((window text-box) ev)
  (symbol-macrolet ((input-offset (text-box-cursor-offset window)))
    (let ((input-text (text-box-text window)))
      (cond ((and (characterp ev) (graphic-char-p ev))
             (vector-push-extend ev input-text)
             (replace input-text input-text :start1 (1+ input-offset)
                                            :end1 (length input-text)
                                            :start2 input-offset
                                            :end2 (1- (length input-text)))
             (setf (char input-text input-offset) ev)
             (incf input-offset))
            ((or (eq ev #\rubout) (eq ev #\backspace))
             (when (and (plusp (length input-text)) (plusp input-offset))
               (replace input-text input-text :start1 (1- input-offset)
                                              :end1 (1- (length input-text))
                                              :start2 input-offset
                                              :end2 (length input-text))
               (vector-pop input-text)
               (decf input-offset)))
            ((eq ev :left)
             (when (plusp input-offset)
               (decf input-offset)))
            ((eq ev :right)
             (when (< input-offset (length input-text))
               (incf input-offset)))
            (t (return-from handle-window-event nil))))))

(defmethod present ((window text-box))
  (ti:tputs ti:cursor-address (win-y window) (win-x window))
  (let* ((text (split-sequence:split-sequence #\newline (text-box-text window))))
    (loop :for line :in text
          :for visual-line :from (win-y window)
          :do (ti:tputs ti:cursor-address visual-line (win-x window))
              (write-string (subseq line 0 (min (length line) (win-cols window))))
              (let ((remaining (- (win-cols window) (display-width line))))
                (when (plusp remaining)
                  (ti:tputs ti:erase-chars remaining))))))

(defmethod present :after ((window text-box))
  (when (win-focused-p window)
    (ti:tputs ti:cursor-address
              (win-y window)
              (+ (win-x window) (text-box-cursor-offset window)))))
