;;; Copyright 2013 Andrey Fainer

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

;;; Commentary:

;;; Code:

(in-package #:sharper-test)

;; (walk-image-box tile img (get-loc1 img) (get-loc2 img)
;;   (prepare-smooth tile)
;;   (smooth-tile tile))

(defun write-tiles (node &rest tiles)
  "Write TILES to NODE.
Each element of TILES is a cons (RES . VECTOR)"
  (mapc #'(lambda (tl)
            (setf (sharper::node-tile node (car tl)) (cdr tl)))
        tiles))

;;; The test dtree is not created
(defun str2vec (s)
  "Convert the string S to a byte vector."
  (map '(vector (unsigned-byte 8)) #'(lambda (c) (char-code c)) s))

(defun vec2str (v)
  "Convert the byte vector V to a string."
  (if v (map 'string #'(lambda (c) (code-char c)) v)))

(defun sort-tiles (tiles)
  (mapcar #'(lambda (tl)
              (nconc (butlast tl 2)
                     (multiple-value-list
                      (apply #'sort-box (last tl 2)))))
          tiles))

(defmacro with-root-tiles (&body body)
  `(let (tiles)
     (with-root ,@body)
     (apply #'values (sort-tiles (nreverse tiles)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sort-form-image (form)
    `(sort ,form
           #'<
           :key #'(lambda (l)
                    (sharper::unfold (nth 3 l))))))

;;; Does the order matter?
(defmacro deftest-sort-image (name form &rest values)
  "TODO Docstring"
  (macrolet ((sortm (values)
               (sort-form-image values)))
    `(deftest ,name ,(sort-form-image form) ,(sortm values))))

;;; Root is not created.
(deftest walk-image-box-test1
    (let (tiles)
      (walk-image-box "/tmp/root/" (locat 3 1 4 5) (locat 3 7 2 4)
        (push (list (vec2str tile)
                    tileres tileloc
                    tilel1 tilel2)
              tiles))
      (car tiles))
  (nil 3 (3 0 0 0) (3 1 4 5) (3 7 2 4))) ; Do we need sorted box here?

;;; There is root node only
(deftest walk-image-box-test2
    (with-root-tiles
      (ensure-directories-exist "/tmp/root/")
      (write-node "/tmp/root/" 5 3)
      (dolist (r (list 1 3 4))
        (setf (sharper::node-tile "/tmp/root/" r)
              (str2vec (format nil "This is tile number ~D" r))))
      (let ((l1 (locat 5 1 24 11))
            (l2 (locat 5 6 17 31)))
        (for (r 1 (<= r 5) (incf r))
          (walk-image-box "/tmp/root/" (resol r l1) (resol r l2)
            (push (list (vec2str tile)
                        tileres tileloc
                        tilel1 tilel2)
                  tiles)
            (push (list 'notile
                        tileres tileloc
                        tilel1 tilel2)
                  tiles)))))
  ("This is tile number 1" 1 (1 0 0 0) (1 0 1 0) (1 0 1 1))
  (notile 2 (2 0 0 0) (2 0 2 1) (2 0 3 3))
  ("This is tile number 3" 3 (3 0 0 0) (3 0 4 2) (3 1 6 7))
  ("This is tile number 4" 4 (4 0 0 0) (4 0 8 5) (4 3 12 15))
  (notile 5 (5 0 0 0) (5 1 17 11) (5 6 24 31)))

;;; Reading at resolution higher than the root resolution
(deftest-sort-image walk-image-box-test3
    (with-root-tiles
      (ensure-directories-exist "/tmp/root/")
      (write-node "/tmp/root/" 4 3)
      (dolist (r (list 1 3 4))
        (setf (sharper::node-tile "/tmp/root/" r)
              (str2vec (format nil "This is tile number ~D" r))))
      (let ((l1 (locat 5 1 24 11))
            (l2 (locat 5 6 17 31)))
        (for (r 1 (<= r 5) (incf r))
          (walk-image-box "/tmp/root/" (resol r l1) (resol r l2)
            (push (list (vec2str tile)
                        tileres tileloc
                        tilel1 tilel2)
                  tiles)
            (push (list 'notile
                        tileres tileloc
                        tilel1 tilel2)
                  tiles)))))
  ("This is tile number 1" 1 (1 0 0 0) (1 0 1 0) (1 0 1 1))
  (NOTILE 2 (2 0 0 0) (2 0 2 1) (2 0 3 3))
  ("This is tile number 3" 3 (3 0 0 0) (3 0 4 2) (3 1 6 7))
  ("This is tile number 4" 4 (4 0 0 0) (4 0 8 5) (4 3 12 15))
  (NOTILE 1 (5 6 24 30) (5 6 24 30) (5 6 24 31))
  (NOTILE 1 (5 4 24 30) (5 4 24 30) (5 5 24 31))
  (NOTILE 1 (5 2 24 30) (5 2 24 30) (5 3 24 31))
  (NOTILE 1 (5 0 24 30) (5 1 24 30) (5 1 24 31))
  (NOTILE 1 (5 6 22 30) (5 6 22 30) (5 6 23 31))
  (NOTILE 1 (5 4 22 30) (5 4 22 30) (5 5 23 31))
  (NOTILE 1 (5 2 22 30) (5 2 22 30) (5 3 23 31))
  (NOTILE 1 (5 0 22 30) (5 1 22 30) (5 1 23 31))
  (NOTILE 1 (5 6 20 30) (5 6 20 30) (5 6 21 31))
  (NOTILE 1 (5 4 20 30) (5 4 20 30) (5 5 21 31))
  (NOTILE 1 (5 2 20 30) (5 2 20 30) (5 3 21 31))
  (NOTILE 1 (5 0 20 30) (5 1 20 30) (5 1 21 31))
  (NOTILE 1 (5 6 18 30) (5 6 18 30) (5 6 19 31))
  (NOTILE 1 (5 4 18 30) (5 4 18 30) (5 5 19 31))
  (NOTILE 1 (5 2 18 30) (5 2 18 30) (5 3 19 31))
  (NOTILE 1 (5 0 18 30) (5 1 18 30) (5 1 19 31))
  (NOTILE 1 (5 6 16 30) (5 6 17 30) (5 6 17 31))
  (NOTILE 1 (5 4 16 30) (5 4 17 30) (5 5 17 31))
  (NOTILE 1 (5 2 16 30) (5 2 17 30) (5 3 17 31))
  (NOTILE 1 (5 0 16 30) (5 1 17 30) (5 1 17 31))
  (NOTILE 1 (5 6 24 28) (5 6 24 28) (5 6 24 29))
  (NOTILE 1 (5 4 24 28) (5 4 24 28) (5 5 24 29))
  (NOTILE 1 (5 2 24 28) (5 2 24 28) (5 3 24 29))
  (NOTILE 1 (5 0 24 28) (5 1 24 28) (5 1 24 29))
  (NOTILE 1 (5 6 22 28) (5 6 22 28) (5 6 23 29))
  (NOTILE 1 (5 4 22 28) (5 4 22 28) (5 5 23 29))
  (NOTILE 1 (5 2 22 28) (5 2 22 28) (5 3 23 29))
  (NOTILE 1 (5 0 22 28) (5 1 22 28) (5 1 23 29))
  (NOTILE 1 (5 6 20 28) (5 6 20 28) (5 6 21 29))
  (NOTILE 1 (5 4 20 28) (5 4 20 28) (5 5 21 29))
  (NOTILE 1 (5 2 20 28) (5 2 20 28) (5 3 21 29))
  (NOTILE 1 (5 0 20 28) (5 1 20 28) (5 1 21 29))
  (NOTILE 1 (5 6 18 28) (5 6 18 28) (5 6 19 29))
  (NOTILE 1 (5 4 18 28) (5 4 18 28) (5 5 19 29))
  (NOTILE 1 (5 2 18 28) (5 2 18 28) (5 3 19 29))
  (NOTILE 1 (5 0 18 28) (5 1 18 28) (5 1 19 29))
  (NOTILE 1 (5 6 16 28) (5 6 17 28) (5 6 17 29))
  (NOTILE 1 (5 4 16 28) (5 4 17 28) (5 5 17 29))
  (NOTILE 1 (5 2 16 28) (5 2 17 28) (5 3 17 29))
  (NOTILE 1 (5 0 16 28) (5 1 17 28) (5 1 17 29))
  (NOTILE 1 (5 6 24 26) (5 6 24 26) (5 6 24 27))
  (NOTILE 1 (5 4 24 26) (5 4 24 26) (5 5 24 27))
  (NOTILE 1 (5 2 24 26) (5 2 24 26) (5 3 24 27))
  (NOTILE 1 (5 0 24 26) (5 1 24 26) (5 1 24 27))
  (NOTILE 1 (5 6 22 26) (5 6 22 26) (5 6 23 27))
  (NOTILE 1 (5 4 22 26) (5 4 22 26) (5 5 23 27))
  (NOTILE 1 (5 2 22 26) (5 2 22 26) (5 3 23 27))
  (NOTILE 1 (5 0 22 26) (5 1 22 26) (5 1 23 27))
  (NOTILE 1 (5 6 20 26) (5 6 20 26) (5 6 21 27))
  (NOTILE 1 (5 4 20 26) (5 4 20 26) (5 5 21 27))
  (NOTILE 1 (5 2 20 26) (5 2 20 26) (5 3 21 27))
  (NOTILE 1 (5 0 20 26) (5 1 20 26) (5 1 21 27))
  (NOTILE 1 (5 6 18 26) (5 6 18 26) (5 6 19 27))
  (NOTILE 1 (5 4 18 26) (5 4 18 26) (5 5 19 27))
  (NOTILE 1 (5 2 18 26) (5 2 18 26) (5 3 19 27))
  (NOTILE 1 (5 0 18 26) (5 1 18 26) (5 1 19 27))
  (NOTILE 1 (5 6 16 26) (5 6 17 26) (5 6 17 27))
  (NOTILE 1 (5 4 16 26) (5 4 17 26) (5 5 17 27))
  (NOTILE 1 (5 2 16 26) (5 2 17 26) (5 3 17 27))
  (NOTILE 1 (5 0 16 26) (5 1 17 26) (5 1 17 27))
  (NOTILE 1 (5 6 24 24) (5 6 24 24) (5 6 24 25))
  (NOTILE 1 (5 4 24 24) (5 4 24 24) (5 5 24 25))
  (NOTILE 1 (5 2 24 24) (5 2 24 24) (5 3 24 25))
  (NOTILE 1 (5 0 24 24) (5 1 24 24) (5 1 24 25))
  (NOTILE 1 (5 6 22 24) (5 6 22 24) (5 6 23 25))
  (NOTILE 1 (5 4 22 24) (5 4 22 24) (5 5 23 25))
  (NOTILE 1 (5 2 22 24) (5 2 22 24) (5 3 23 25))
  (NOTILE 1 (5 0 22 24) (5 1 22 24) (5 1 23 25))
  (NOTILE 1 (5 6 20 24) (5 6 20 24) (5 6 21 25))
  (NOTILE 1 (5 4 20 24) (5 4 20 24) (5 5 21 25))
  (NOTILE 1 (5 2 20 24) (5 2 20 24) (5 3 21 25))
  (NOTILE 1 (5 0 20 24) (5 1 20 24) (5 1 21 25))
  (NOTILE 1 (5 6 18 24) (5 6 18 24) (5 6 19 25))
  (NOTILE 1 (5 4 18 24) (5 4 18 24) (5 5 19 25))
  (NOTILE 1 (5 2 18 24) (5 2 18 24) (5 3 19 25))
  (NOTILE 1 (5 0 18 24) (5 1 18 24) (5 1 19 25))
  (NOTILE 1 (5 6 16 24) (5 6 17 24) (5 6 17 25))
  (NOTILE 1 (5 4 16 24) (5 4 17 24) (5 5 17 25))
  (NOTILE 1 (5 2 16 24) (5 2 17 24) (5 3 17 25))
  (NOTILE 1 (5 0 16 24) (5 1 17 24) (5 1 17 25))
  (NOTILE 1 (5 6 24 22) (5 6 24 22) (5 6 24 23))
  (NOTILE 1 (5 4 24 22) (5 4 24 22) (5 5 24 23))
  (NOTILE 1 (5 2 24 22) (5 2 24 22) (5 3 24 23))
  (NOTILE 1 (5 0 24 22) (5 1 24 22) (5 1 24 23))
  (NOTILE 1 (5 6 22 22) (5 6 22 22) (5 6 23 23))
  (NOTILE 1 (5 4 22 22) (5 4 22 22) (5 5 23 23))
  (NOTILE 1 (5 2 22 22) (5 2 22 22) (5 3 23 23))
  (NOTILE 1 (5 0 22 22) (5 1 22 22) (5 1 23 23))
  (NOTILE 1 (5 6 20 22) (5 6 20 22) (5 6 21 23))
  (NOTILE 1 (5 4 20 22) (5 4 20 22) (5 5 21 23))
  (NOTILE 1 (5 2 20 22) (5 2 20 22) (5 3 21 23))
  (NOTILE 1 (5 0 20 22) (5 1 20 22) (5 1 21 23))
  (NOTILE 1 (5 6 18 22) (5 6 18 22) (5 6 19 23))
  (NOTILE 1 (5 4 18 22) (5 4 18 22) (5 5 19 23))
  (NOTILE 1 (5 2 18 22) (5 2 18 22) (5 3 19 23))
  (NOTILE 1 (5 0 18 22) (5 1 18 22) (5 1 19 23))
  (NOTILE 1 (5 6 16 22) (5 6 17 22) (5 6 17 23))
  (NOTILE 1 (5 4 16 22) (5 4 17 22) (5 5 17 23))
  (NOTILE 1 (5 2 16 22) (5 2 17 22) (5 3 17 23))
  (NOTILE 1 (5 0 16 22) (5 1 17 22) (5 1 17 23))
  (NOTILE 1 (5 6 24 20) (5 6 24 20) (5 6 24 21))
  (NOTILE 1 (5 4 24 20) (5 4 24 20) (5 5 24 21))
  (NOTILE 1 (5 2 24 20) (5 2 24 20) (5 3 24 21))
  (NOTILE 1 (5 0 24 20) (5 1 24 20) (5 1 24 21))
  (NOTILE 1 (5 6 22 20) (5 6 22 20) (5 6 23 21))
  (NOTILE 1 (5 4 22 20) (5 4 22 20) (5 5 23 21))
  (NOTILE 1 (5 2 22 20) (5 2 22 20) (5 3 23 21))
  (NOTILE 1 (5 0 22 20) (5 1 22 20) (5 1 23 21))
  (NOTILE 1 (5 6 20 20) (5 6 20 20) (5 6 21 21))
  (NOTILE 1 (5 4 20 20) (5 4 20 20) (5 5 21 21))
  (NOTILE 1 (5 2 20 20) (5 2 20 20) (5 3 21 21))
  (NOTILE 1 (5 0 20 20) (5 1 20 20) (5 1 21 21))
  (NOTILE 1 (5 6 18 20) (5 6 18 20) (5 6 19 21))
  (NOTILE 1 (5 4 18 20) (5 4 18 20) (5 5 19 21))
  (NOTILE 1 (5 2 18 20) (5 2 18 20) (5 3 19 21))
  (NOTILE 1 (5 0 18 20) (5 1 18 20) (5 1 19 21))
  (NOTILE 1 (5 6 16 20) (5 6 17 20) (5 6 17 21))
  (NOTILE 1 (5 4 16 20) (5 4 17 20) (5 5 17 21))
  (NOTILE 1 (5 2 16 20) (5 2 17 20) (5 3 17 21))
  (NOTILE 1 (5 0 16 20) (5 1 17 20) (5 1 17 21))
  (NOTILE 1 (5 6 24 18) (5 6 24 18) (5 6 24 19))
  (NOTILE 1 (5 4 24 18) (5 4 24 18) (5 5 24 19))
  (NOTILE 1 (5 2 24 18) (5 2 24 18) (5 3 24 19))
  (NOTILE 1 (5 0 24 18) (5 1 24 18) (5 1 24 19))
  (NOTILE 1 (5 6 22 18) (5 6 22 18) (5 6 23 19))
  (NOTILE 1 (5 4 22 18) (5 4 22 18) (5 5 23 19))
  (NOTILE 1 (5 2 22 18) (5 2 22 18) (5 3 23 19))
  (NOTILE 1 (5 0 22 18) (5 1 22 18) (5 1 23 19))
  (NOTILE 1 (5 6 20 18) (5 6 20 18) (5 6 21 19))
  (NOTILE 1 (5 4 20 18) (5 4 20 18) (5 5 21 19))
  (NOTILE 1 (5 2 20 18) (5 2 20 18) (5 3 21 19))
  (NOTILE 1 (5 0 20 18) (5 1 20 18) (5 1 21 19))
  (NOTILE 1 (5 6 18 18) (5 6 18 18) (5 6 19 19))
  (NOTILE 1 (5 4 18 18) (5 4 18 18) (5 5 19 19))
  (NOTILE 1 (5 2 18 18) (5 2 18 18) (5 3 19 19))
  (NOTILE 1 (5 0 18 18) (5 1 18 18) (5 1 19 19))
  (NOTILE 1 (5 6 16 18) (5 6 17 18) (5 6 17 19))
  (NOTILE 1 (5 4 16 18) (5 4 17 18) (5 5 17 19))
  (NOTILE 1 (5 2 16 18) (5 2 17 18) (5 3 17 19))
  (NOTILE 1 (5 0 16 18) (5 1 17 18) (5 1 17 19))
  (NOTILE 1 (5 6 24 16) (5 6 24 16) (5 6 24 17))
  (NOTILE 1 (5 4 24 16) (5 4 24 16) (5 5 24 17))
  (NOTILE 1 (5 2 24 16) (5 2 24 16) (5 3 24 17))
  (NOTILE 1 (5 0 24 16) (5 1 24 16) (5 1 24 17))
  (NOTILE 1 (5 6 22 16) (5 6 22 16) (5 6 23 17))
  (NOTILE 1 (5 4 22 16) (5 4 22 16) (5 5 23 17))
  (NOTILE 1 (5 2 22 16) (5 2 22 16) (5 3 23 17))
  (NOTILE 1 (5 0 22 16) (5 1 22 16) (5 1 23 17))
  (NOTILE 1 (5 6 20 16) (5 6 20 16) (5 6 21 17))
  (NOTILE 1 (5 4 20 16) (5 4 20 16) (5 5 21 17))
  (NOTILE 1 (5 2 20 16) (5 2 20 16) (5 3 21 17))
  (NOTILE 1 (5 0 20 16) (5 1 20 16) (5 1 21 17))
  (NOTILE 1 (5 6 18 16) (5 6 18 16) (5 6 19 17))
  (NOTILE 1 (5 4 18 16) (5 4 18 16) (5 5 19 17))
  (NOTILE 1 (5 2 18 16) (5 2 18 16) (5 3 19 17))
  (NOTILE 1 (5 0 18 16) (5 1 18 16) (5 1 19 17))
  (NOTILE 1 (5 6 16 16) (5 6 17 16) (5 6 17 17))
  (NOTILE 1 (5 4 16 16) (5 4 17 16) (5 5 17 17))
  (NOTILE 1 (5 2 16 16) (5 2 17 16) (5 3 17 17))
  (NOTILE 1 (5 0 16 16) (5 1 17 16) (5 1 17 17))
  (NOTILE 1 (5 6 24 14) (5 6 24 14) (5 6 24 15))
  (NOTILE 1 (5 4 24 14) (5 4 24 14) (5 5 24 15))
  (NOTILE 1 (5 2 24 14) (5 2 24 14) (5 3 24 15))
  (NOTILE 1 (5 0 24 14) (5 1 24 14) (5 1 24 15))
  (NOTILE 1 (5 6 22 14) (5 6 22 14) (5 6 23 15))
  (NOTILE 1 (5 4 22 14) (5 4 22 14) (5 5 23 15))
  (NOTILE 1 (5 2 22 14) (5 2 22 14) (5 3 23 15))
  (NOTILE 1 (5 0 22 14) (5 1 22 14) (5 1 23 15))
  (NOTILE 1 (5 6 20 14) (5 6 20 14) (5 6 21 15))
  (NOTILE 1 (5 4 20 14) (5 4 20 14) (5 5 21 15))
  (NOTILE 1 (5 2 20 14) (5 2 20 14) (5 3 21 15))
  (NOTILE 1 (5 0 20 14) (5 1 20 14) (5 1 21 15))
  (NOTILE 1 (5 6 18 14) (5 6 18 14) (5 6 19 15))
  (NOTILE 1 (5 4 18 14) (5 4 18 14) (5 5 19 15))
  (NOTILE 1 (5 2 18 14) (5 2 18 14) (5 3 19 15))
  (NOTILE 1 (5 0 18 14) (5 1 18 14) (5 1 19 15))
  (NOTILE 1 (5 6 16 14) (5 6 17 14) (5 6 17 15))
  (NOTILE 1 (5 4 16 14) (5 4 17 14) (5 5 17 15))
  (NOTILE 1 (5 2 16 14) (5 2 17 14) (5 3 17 15))
  (NOTILE 1 (5 0 16 14) (5 1 17 14) (5 1 17 15))
  (NOTILE 1 (5 6 24 12) (5 6 24 12) (5 6 24 13))
  (NOTILE 1 (5 4 24 12) (5 4 24 12) (5 5 24 13))
  (NOTILE 1 (5 2 24 12) (5 2 24 12) (5 3 24 13))
  (NOTILE 1 (5 0 24 12) (5 1 24 12) (5 1 24 13))
  (NOTILE 1 (5 6 22 12) (5 6 22 12) (5 6 23 13))
  (NOTILE 1 (5 4 22 12) (5 4 22 12) (5 5 23 13))
  (NOTILE 1 (5 2 22 12) (5 2 22 12) (5 3 23 13))
  (NOTILE 1 (5 0 22 12) (5 1 22 12) (5 1 23 13))
  (NOTILE 1 (5 6 20 12) (5 6 20 12) (5 6 21 13))
  (NOTILE 1 (5 4 20 12) (5 4 20 12) (5 5 21 13))
  (NOTILE 1 (5 2 20 12) (5 2 20 12) (5 3 21 13))
  (NOTILE 1 (5 0 20 12) (5 1 20 12) (5 1 21 13))
  (NOTILE 1 (5 6 18 12) (5 6 18 12) (5 6 19 13))
  (NOTILE 1 (5 4 18 12) (5 4 18 12) (5 5 19 13))
  (NOTILE 1 (5 2 18 12) (5 2 18 12) (5 3 19 13))
  (NOTILE 1 (5 0 18 12) (5 1 18 12) (5 1 19 13))
  (NOTILE 1 (5 6 16 12) (5 6 17 12) (5 6 17 13))
  (NOTILE 1 (5 4 16 12) (5 4 17 12) (5 5 17 13))
  (NOTILE 1 (5 2 16 12) (5 2 17 12) (5 3 17 13))
  (NOTILE 1 (5 0 16 12) (5 1 17 12) (5 1 17 13))
  (NOTILE 1 (5 6 24 10) (5 6 24 11) (5 6 24 11))
  (NOTILE 1 (5 4 24 10) (5 4 24 11) (5 5 24 11))
  (NOTILE 1 (5 2 24 10) (5 2 24 11) (5 3 24 11))
  (NOTILE 1 (5 0 24 10) (5 1 24 11) (5 1 24 11))
  (NOTILE 1 (5 6 22 10) (5 6 22 11) (5 6 23 11))
  (NOTILE 1 (5 4 22 10) (5 4 22 11) (5 5 23 11))
  (NOTILE 1 (5 2 22 10) (5 2 22 11) (5 3 23 11))
  (NOTILE 1 (5 0 22 10) (5 1 22 11) (5 1 23 11))
  (NOTILE 1 (5 6 20 10) (5 6 20 11) (5 6 21 11))
  (NOTILE 1 (5 4 20 10) (5 4 20 11) (5 5 21 11))
  (NOTILE 1 (5 2 20 10) (5 2 20 11) (5 3 21 11))
  (NOTILE 1 (5 0 20 10) (5 1 20 11) (5 1 21 11))
  (NOTILE 1 (5 6 18 10) (5 6 18 11) (5 6 19 11))
  (NOTILE 1 (5 4 18 10) (5 4 18 11) (5 5 19 11))
  (NOTILE 1 (5 2 18 10) (5 2 18 11) (5 3 19 11))
  (NOTILE 1 (5 0 18 10) (5 1 18 11) (5 1 19 11))
  (NOTILE 1 (5 6 16 10) (5 6 17 11) (5 6 17 11))
  (NOTILE 1 (5 4 16 10) (5 4 17 11) (5 5 17 11))
  (NOTILE 1 (5 2 16 10) (5 2 17 11) (5 3 17 11))
  (NOTILE 1 (5 0 16 10) (5 1 17 11) (5 1 17 11)))

;;; There is no lower resolution
;; (deftest walk-image-box-read-test3
;;   (with-root
;;     (ensure-directories-exist "/tmp/root/")
;;     (write-node "/tmp/root/" 4 3)
;;     (setf (sharper::node-tile "/tmp/root/" 3)
;;           (str2vec "This is tile number 3"))
;;     (let (tiles)
;;       (walk-image-box
;;           tile "/tmp/root/"
;;           (locat 2 1 2 3) (locat 2 4 5 6)
;;         (push (list (vec2str tile)
;;                     tile-res tile-loc
;;                     tile-bl1 tile-bl2)
;;               tiles)
;;         (push (list tile-res tile-loc
;;                     tile-bl1 tile-bl2
;;                     tile-low-res
;;                     tile-low-loc
;;                     tile-loc-low)
;;               tiles))
;;       tiles))
;;   (2 (2 0 0 0) (2 1 2 3) (2 4 5 6) nil nil nil))

;; ;;; Read tiles in kids
;; (deftest walk-image-box-read-test4
;;     (with-root
;;       (ensure-directories-exist "/tmp/root/")
;;       (write-node "/tmp/root/" 4 3)
;;       (let (tiles-read
;;             tiles-root
;;             tiles-low
;;             (l1 (locat 7 3 9 17))
;;             (l2 (locat 7 76 43 100))
;;             (kidsnotiles (mapcar #'locat
;;                                  (make-list 7 :initial-element 4)
;;                                  '((0 1 2) (1 1 2) (5 2 5) (6 2 5)
;;                                    (5 3 6) (6 3 7) (7 5 12))))
;;             (kidslowtiles (mapcar #'locat
;;                                  (make-list 6 :initial-element 4)
;;                                  '((9 1 3) (9 2 3) (1 3 9)
;;                                    (2 3 9) (1 4 8) (9 5 12)))))
;;         (create-kids l1 l2 3 "/tmp/root/" 4)
;;         ;; Kids without tiles and no low res
;;         ;;
;;         ;; Kids without tiles and low res in root
;;         ;; ((0 1 ) (1 1 ) (5 2 ) (6 2 ) (5 3 ) (6 3 ) (7 5 ))
;;         ;; Kids with tiles not on the target resolution
;;         ;; ((9 1 ) (9 2 ) (1 3 ) (2 3 ) (1 4 ) (9 5 ))
;;         ;; Kids without tiles and no low res
;;         ;; Out of the box
;;         (setf (node-tile "/tmp/root/" 3)
;;               (str2vec "This is root tile at resolution 3!"))
;;         (walk-box (resol 4 l1) (resol 4 l2)
;;                   #'(lambda (l)
;;                       (cond
;;                         ((member l kidsnotiles))
;;                         ((member l kidslowtiles)
;;                          (setf (node-tile (find-node "/tmp/root/" l) 1)
;;                                (str2vec
;;                                 (format nil
;;                                         "This is a low res tile at loc ~A"
;;                                         l))))
;;                         (t (setf (node-tile (find-node "/tmp/root/" l) 1)
;;                                  (str2vec
;;                                   (format nil
;;                                           "This is a tile at loc ~A" l)))))))
;;         (walk-image-box))))

;; (defmacro with-root-res (res &body body)
;;   "Execute BODY with temporary image and node resolution RES."
;;   (let (oldres (gensym))
;;     `(let ((,oldres sharper::*new-node-resolution*))
;;        (unwind-protect
;;             (with-root
;;               (setq sharper::*new-node-resolution* ,res)
;;               ,@body)
;;          (setq sharper::*new-node-resolution* ,oldres)))))
