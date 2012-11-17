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

(in-package #:sharper-test)

(defun replace-gensyms (gsform form)
  "TODO Add docstring"
  (cond ((and (symbolp gsform)
              (symbolp form)
              (not (symbol-package gsform)))
         (intern (subseq (symbol-name gsform) 0
                         (length (symbol-name form)))))
        ((and (consp gsform) (consp form))
         (cons (replace-gensyms (car gsform) (car form))
               (replace-gensyms (cdr gsform) (cdr form))))
        (t gsform)))

(defmacro defmacrotest (name form expansion value)
  "TODO Add docstring"
  `(progn
     (deftest ,(alexandria:symbolicate name 'expansion)
         (replace-gensyms (macroexpand-1 ',form) ',expansion)
       ,expansion)
     (deftest ,(alexandria:symbolicate name 'call)
         ,form
       ,value)))

(defvar *testvals* () "`for' tests values")

(defmacrotest fortest1
    (for (var (+ 2 3)
              (< var 10)
              (1+ var)
              :prebegin (setq *testvals* `((prebegin ,var)))
              :postbegin (push `(postbegin ,var) *testvals*)
              :preend (push `(preend ,var) *testvals*)
              :postend (nreverse (push `(postend ,var) *testvals*)))
      (push var *testvals*)
      nil)
  (let* ((var (+ 2 3))
         (gpreend (let ((var var))
                    #'(lambda ()
                        (push `(preend ,var) *testvals*)
                        nil var)))
         (gbody (let ((var var))
                  #'(lambda ()
                      (push var *testvals*)
                      nil
                      (push `(postbegin ,var) *testvals*)
                      var))))
    (setq *testvals* `((prebegin ,var)))
    (when (< var 10)
      (setq var (1+ var))
      (while (< var 10)
        (setq gpreend
              (let ((var (funcall gbody)))
                #'(lambda ()
                    (push `(preend ,var) *testvals*)
                    nil var))
              gbody
              (let ((var var))
                #'(lambda ()
                    (push var *testvals*)
                    nil nil var)))
        (setq var (1+ var)))
      (funcall gpreend)
      (setq var (funcall gbody)))
    (nreverse (push `(postend ,var) *testvals*)))
  ((PREBEGIN 5) 5 (POSTBEGIN 5) 6
   7 8 (PREEND 8) 9 (POSTEND 9)))

(deftest fortest2
    (for (var (+ 2 3)
              (> var 10)
              (1+ var)
              :prebegin (setq *testvals* `((prebegin ,var)))
              :postbegin (push `(postbegin ,var) *testvals*)
              :preend (push `(preend ,var) *testvals*)
              :postend (nreverse (push `(postend ,var) *testvals*)))
      (push var *testvals*)
      nil)
  ((PREBEGIN 5) (POSTEND 5)))

(deftest fortest3
    (for (var '(a b c d)
              (consp var)
              (cdr var)
              :prebegin (setq *testvals* `((prebegin ,var)))
              :postbegin (push `(postbegin ,var) *testvals*)
              :preend (push `(preend ,var) *testvals*)
              :postend (nreverse (push `(postend ,var) *testvals*)))
      (push var *testvals*))
  ((PREBEGIN (A B C D)) (A B C D) (POSTBEGIN (A B C D))
   (B C D) (C D) (PREEND (C D)) (D) (POSTEND (D))))

(deftest fortest4
    (for (var (+ 2 3)
              (< var 6)
              (1+ var)
              :prebegin (setq *testvals* `((prebegin ,var)))
              :postbegin (push `(postbegin ,var) *testvals*)
              :preend (push `(preend ,var) *testvals*)
              :postend (nreverse (push `(postend ,var) *testvals*)))
      (push var *testvals*)
      nil)
  ((PREBEGIN 5) (PREEND 5) 5 (POSTBEGIN 5) (POSTEND 5)))
