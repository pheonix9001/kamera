(defpackage kamera
  (:use :cl)
  (:export make-variable))
(in-package :kamera)

(defclass logic-var ()
  ((name
    :accessor var-name
    :initarg :name
    :type symbol))
  (:documentation
   "A logic variable

Note that logic variables do not store their values. They are just mere placeholders. Use `run' to evaluate
an expression and get back a value"))

(defun make-variable (name)
  (make-instance 'logic-var :name name))

(defmethod print-object ((var logic-var) stream)
  (format stream "$~A" (var-name var)))

(define-condition unify-failed ()
  ((val1 :reader unify-failed-1)
   (val2 :reader unify-failed-2))
  (:report "Failed to unify variables"))
(defgeneric unify (val1 val2)
  (:documentation "Unify two values to produce a list of substitutions

Throws `unify-failed' if they cant be unified"))

(defmethod unify (val1 val2)
  (if (equal val1 val2)
    '()
     (error 'unify-failed :val1 val1 :val2 val2)))

(defmethod unify ((var logic-var) val)
  (list (cons var val)))

(defmethod unify ((l1 cons) (l2 cons))
  (nconc
    (unify (car l1) (car l2))
    (unify (cdr l1) (cdr l2))))
