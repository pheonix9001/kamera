(defpackage kamera
  (:use :cl)
  (:export
    make-variable
    unify-failed
    ?-
    unify
    ==)
  (:local-nicknames (:util :alexandria)))
(in-package :kamera)

(defclass logic-var ()
  ((name
    :accessor var-name
    :initarg :name
    :type symbol))
  (:documentation
   "A logic variable

Note that logic variables do not store their values. They are just mere placeholders. Use `run` to evaluate
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
  (:documentation "Unify two values returning the unified result

Throws `unify-failed` if they cant be unified"))
(setf (fdefinition '==) #'unify)

; A table with a prototype table
(defstruct proto-table
  "A hash-table with a prototype hash-table"
  parent
  (curr (make-hash-table)))
(defun getproto (key table)
  (or (gethash key (proto-table-curr table))
      (and
       (proto-table-parent table)
       (getproto key (proto-table-parent table)))))
(defsetf getproto (key table) (val)
 `(setf (gethash ,key (proto-table-curr ,table)) ,val))

(defvar *curr-bindings* (make-proto-table)
  "Mapping of variables to their values")
#+nil
(setf *curr-bindings* (make-proto-table))

(defmethod unify (val1 val2)
  (if (equal val1 val2)
    val1
    (error 'unify-failed :val1 val1 :val2 val2)))

(defmethod unify ((var logic-var) new)
  (setf (getproto var *curr-bindings*)
        (util:if-let (val (getproto var *curr-bindings*))
          (unify val new)
          new)))

(defmethod unify (val (var logic-var))
  (unify var val))

(defun ?- (val)
  ; Check if more useful value exists
  (if (typep val 'logic-var)
      (?- (getproto val *curr-bindings*))
      val))

#+nil
(progn
  (defvar *x* (make-variable 'x))
  (defvar *y* (make-variable 'y)))
#+nil
(progn
  (unify *x* *y*)
  (unify *y* 10))

(defmethod unify ((l1 cons) (l2 cons))
  (cons
    (unify (car l1) (car l2))
    (unify (cdr l1) (cdr l2))))
