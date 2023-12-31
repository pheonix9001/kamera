(defpackage kamera
  (:use :cl :iterate)
  (:export
    make-variable
    unify-failed
    ?-
    unify
    ==
    fresh)
  (:local-nicknames (:a :alexandria)))
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

(define-condition unify-failed ()
  ((val1 :reader unify-failed-1)
   (val2 :reader unify-failed-2))
  (:report "Failed to unify variables"))
(defgeneric unify (val1 val2)
  (:documentation "Unify two values returning the unified result

Throws `unify-failed` if they cant be unified"))
(setf (fdefinition '==) #'unify)

(defun bind-variable (var val)
  (setf *solutions*
    (mapcan
      (lambda (soln)
        (a:if-let (old (getproto var soln))
          (handler-case (unify old val)
            (unify-failed () '())
            (:no-error (res)
              (setf (getproto var soln) res)
              (list soln)))

          (progn
            (setf (getproto var soln) val)
            (list soln))))
      *solutions*)))

(defun copy-solutions (solutions)
  (mapcar (lambda (soln) (make-proto-table :parent soln)) solutions))

(defmacro fresh ((&rest vars) &body body)
  "Creates logic variables `vars` within `body`"
  `(let ((*solutions* (copy-solutions *solutions*))
          ,@(loop for var in vars
                  collect `(,var (make-variable ',var))))
     ,@body))

(defparameter *solutions* (list (make-proto-table))
  "Mapping of variables to their values")

(defmethod unify (val1 val2)
  (if (equal val1 val2)
    val1
    (error 'unify-failed :val1 val1 :val2 val2)))

(defmethod unify ((var logic-var) val)
  (bind-variable var val))

(defmethod unify (val (var logic-var))
  (unify var val))

(defun ?- (val)
  (iter (for soln in *solutions*)
        ; Check if more useful value exists
        (if (typep val 'logic-var)
            (a:when-let ((res (getproto val soln)))
              (appending (?- res)))
            (collect val))))

(defmacro conj (&rest clauses)
  `(progn
     ,@clauses))

(defmacro disj (&rest clauses)
  `(setf *solutions*
     (append
       ,(iter
         (for c in clauses)
         (collect
           `(let ((*solutions* (copy-solutions *solutions*)))
              ,c
              *solutions*))))))

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
