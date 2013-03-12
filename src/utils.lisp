;;; Copyright 2012 Andrey Fainer

;;; This file is part of Sharper.

;;; Sharper is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; Sharper is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with Sharper.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(in-package #:sharper)


;;; Lists

(declaim (inline carl))
(defun carl (l)
  "Return a list with car of the list L."
  (list (car l)))

(declaim (inline cons-if))
(defun cons-if (x y)
  "Return (cons X Y) if X is non-`nil', otherwise return Y."
  (if x (cons x y) y))

(declaim (inline cons-if*))
(defun cons-if* (x y z)
  "Return (cons Y Z) if X is non-`nil', otherwise return Z."
  (if x (cons y z) z))

(declaim (inline last*))
(defun last1 (l)
  "Return car of the last cons of the list L."
  (car (last l)))

(declaim (inline group))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun group (l n)
    "TODO Docstring."
    (when (plusp n)
      (loop with l1 = l
         until (endp l1)
         collect (loop
                    for i from 1 to n
                    until (endp l1)
                    collect (pop l1))))))

;;; Control flow

(defmacro aif (test then &optional else)
  "Assign TEST to the variable IT. If IT is non-`nil' evaluate THEN.
Otherwise evaluate ELSE. The name `aif' stands for `anaphoric if'."
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body forms)
  "Assign TEST to the variable IT. If IT is non-`nil' evaluate FORMS.
Otherwise return NIL. The name `awhen' stands for `anaphoric when'."
  `(let ((it ,test))
     (when it ,@forms)))

(defmacro unless* (test &body forms)
  "Return the evaluation result of the form TEST if it is non-`nil'.
Otherwise evaluate FORMS and return the result of the last evaluated
form."
  (let ((val (gensym)))
    `(let ((,val ,test))
       (or ,val (progn ,@forms)))))

;;; Bindings

;;; TODO Check value of TEST in expansion-time. If it is nil or t we
;;; can make the right expansion (equvivalent to `if').
(macrolet
    ((defiflet (let-op)
       `(defmacro ,(symbolicate 'if let-op) (test bindings &body body)
          ,(format nil
"Evaluate BODY with variables bound to then or else values.
Elements of the list BINDINGS are the following list:

  (var then-value [else-value]).

If the test form TEST is non-`nil' a new variable bound to then-value
otherwise to else-value. If else-value is not supported it has value
`nil'.

The form

  (iflet test
       ((var1 then1 else1)
        (var2 then2))
     body)

is equivalent to the form

  (if test
      (~A ((var1 then1)
           (var2 then2))
        body)
      (~A ((var1 else1)
           (var2 nil))
        body))." let-op let-op)
          (let (then else)
            (dolist (b bindings)
              (destructuring-bind (v th &optional el) b
                (push (list v th) then)
                (push (list v el) else)))
            `(if ,test
                 (,',let-op ,(nreverse then) ,@body)
                 (,',let-op ,(nreverse else) ,@body))))))
  (defiflet let)
  (defiflet let*))

(defmacro ifdbind (test then-lambdalist then-form
                   else-lambdalist else-form
                   &body body)
  "Evaluate the forms BODY with destructured THEN-FORM or ELSE-FORM.

If the form TEST is non-`nil' new variables specified in
THEN-LAMBDALIST are bound to the corresponding values in THEN-FORM.
Otherwise variables specified in ELSE-LAMBDALIST are bound to the
corresponding values in ELSE-FORM.

The form

  (ifdbind test
           then-lambdalist then-form
           else-lambdalist else-form
           body)

is equivalent to the form

  (if test
      (destructuring-bind then-lambdalist then-form body)
      (destructuring-bind else-lambdalist else-form body))."
  `(if ,test
       (destructuring-bind ,then-lambdalist ,then-form ,@body)
       (destructuring-bind ,else-lambdalist ,else-form ,@body)))

;;; Loops

(defmacro while (test &body body)
  "Evaluate the forms of BODY while the expression TEST is non-`nil'."
  `(loop
      while ,test
      do (progn ,@body)))

(defmacro for
    ((var begin test next &key prebegin postbegin preend postend)
     &body body)
  "Bind VAR to BEGIN and evaluate BODY while TEST is non-`nil'.

At the end of each iteration change the value of VAR to the value of
the expression NEXT. The bound variable VAR is visible to the forms of
BODY and the expressions TEST and NEXT.

Use expressions PREBEGIN, POSTBEGIN, PREEND, POSTEND as follows.

- Evaluate PREBEGIN just before the first iteration.
- Evaluate POSTBEGIN just after the first iteration.
- Evaluate PREEND just before the last iteration.
- Evaluate POSTEND just after the last iteration.
- If no iterations are performed evaluate PREBEGIN then POSTEND.

TODO Add notice that TEST evaluates in advance against BODY."
  (with-gensyms (gbody gpreend)
    (flet ((lamb (v b &optional e)
             `(let ((,var ,v))
                #'(lambda () ,@b ,e ,var))))
      `(let* ((,var ,begin)
              (,gpreend ,(lamb var (list preend)))
              (,gbody ,(lamb var body postbegin)))
         ,prebegin
         (when ,test
           (setq ,var ,next)
           (while ,test
             (setq ,gpreend ,(lamb `(funcall ,gbody) (list preend))
                   ,gbody ,(lamb var body))
             (setq ,var ,next))
           (funcall ,gpreend)
           (setq ,var (funcall ,gbody)))
         ,postend))))


;;; Strings

(declaim (inline cat))
(defun cat (&rest sequences)
  "Convert SEQUENCES to strings and `concatenate' them."
  (apply #'concatenate 'string
         (mapcar #'string sequences)))

;;; Symbols

(defun sym-eq (s1 s2 &rest more)
  "Compare symbols S1 S2 and MORE regardless of their packages."
  (let ((n (symbol-name s1)))
    (every #'(lambda (s)
               (string= n (symbol-name s)))
           (cons s2 more))))

;;; Pathnames

(declaim (inline dir+file))
(defun dir+file (dir file)
  "Return the pathname DIR appended with the pathname FILE."
  (merge-pathnames file
                   (pathname-as-directory dir)))

(defun dirup (dir)
  "Return the parent directory of the pathname DIR.
Return the pathname DIR without parent directories as the second
value."
  (let ((struct (pathname-directory dir)))
    (values (make-pathname :directory (butlast struct))
            (last1 struct))))

;;; FIXME (pathname-eq "~/tmp/" "/home/user/tmp/") => NIL
(defun pathname-eq (p1 p2 &rest more)
  "Return T if P1 and P2 are equal pathnames.
Both arguments are converted to strings by `namestring' and compared
with `string=' so P1 and P2 can be strings or pathnames. Use the
argument MORE to compare more than two pathnames."
  (let ((s (namestring p1)))
   (every #'(lambda (p)
              (string= s p))
          (mapcar #'namestring (cons p2 more)))))

;;; Numbers

;;; TODO Add clamp-min and clamp-max

(declaim (inline ceil))
(defun ceil (number &optional (divisor 1))
  "if DIVISOR is 1 return (ceiling NUMBER).
Otherwise, return (* NUMBER (ceiling NUMBER/DIVISOR))."
  (if (= divisor 1)
      (ceiling number)
      (- number
         (cadr (multiple-value-list (ceiling number divisor))))))

(declaim (inline mapa-b))
(defun mapa-b (fn a b &optional (step 1))
  "Apply the function FN to the range [A; B] with the optional STEP."
  (loop for i from a to b by step
       collect (funcall fn i)))

(declaim (inline map0-n))
(defun map0-n (fn n &optional (step 1))
  "Apply the function FN to the range [0; N] with the optional STEP."
  (mapa-b fn 0 n step))

(declaim (inline map1-n))
(defun map1-n (fn n &optional (step 1))
  "Apply the function FN to the range [1; N] with the optional STEP."
  (mapa-b fn 1 n step))

;;; Types

(defmacro fcoerce (obj &optional llist &body clauses)
  "TODO Docstring."
  `(let ((it ,obj))
    (cond
      ((functionp it) it)
      ,@(mapcar #'(lambda (c)
                    (if (cdr c)
                        `(,(car c)
                           #'(lambda ,llist ,(cadr c)))
                        `(t #'(lambda ,llist ,(car c)))))
                (group clauses 2))
      ;; FIXME Ignore all vars in llist
      (t #'(lambda ,llist it)))))

;;; IO

(defun map-file (fn filename &optional (if-does-not-exist :error)
                 (end nil end-p))
  "Apply the function FN to each expression in the file FILENAME.
Return a list of applying results.

The argument IF-DOES-NOT-EXIST have the same meaning as the
corresponding argument of the function `open'. Its default value
is :ERROR.

If the argument END is a function apply it on each result of applying
the function FN. Stop reading the file FILENAME if the function END
returns T. If the argument END any other object compare it with each
result using the function `equal'. If the argument END is not used
read the file FILENAME until the end of the file is reached."
  (with-open-file (s filename :direction :input
                     :if-does-not-exist if-does-not-exist)
    (when s
      (let ((eof (gensym))
            (end (fcoerce end (x)
                   end-p (equal end x)
                   (declare (ignore x)))))
        (loop
           for r = (read s nil eof)
           with f
           until (eq r eof)
           do (setq f (funcall fn r))
           until (funcall end f)
           collect f)))))

(defun read-file (filename &optional (if-does-not-exist :error))
  "Read expressions in the file FILENAME until EOF is reached.
Return a list of the expessions. The value of the argument
IF-DOES-NOT-EXIST have the same meaning as the value of the
corresponding argument of the function `open'. Its default value is
:ERROR."
  (map-file #'identity filename if-does-not-exist))

(defun write-file (list filename &optional (if-exists :error)
                   (delimiter #'terpri))
  "Write objects in the list LIST to the file FILENAME.

The value of the argument IF-EXISTS have the same meaning as the value
of the corresponding argument of the function `open'. Its default
value is :ERROR.

DELIMITER is a function that takes a stream. It writes a delimiter to
the file FILENAME after each written object from the list LIST."
  (with-open-file (f filename :direction :output
                     :if-does-not-exist :create
                     :if-exists if-exists)
    (dolist (i list list)
      (write i :stream f)
      (funcall delimiter f))))

(defun file-size (pathname)
  "Return size of the file PATHNAME in bytes."
  (with-open-file (f pathname :direction :input
                     :element-type 'unsigned-byte)
    (file-length f)))
