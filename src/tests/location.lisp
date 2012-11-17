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

(deftest walk-box-test1
    (let ((vals))
      (flet ((mkbnds (suf)
               (list #'(lambda (l)
                         (push (list (symbolicate 'prebegin "-" suf) l) vals))
                     #'(lambda (l)
                         (push (list (symbolicate 'postbegin "-" suf) l) vals))
                     #'(lambda (l)
                         (push (list (symbolicate 'preend "-" suf) l) vals))
                     #'(lambda (l)
                         (push (list (symbolicate 'postend "-" suf) l) vals)))))
        (walk-box (locat 4 3 8 5) (locat 4 9 2 8)
                  #'(lambda (l) (push l vals)) 2
                  (mkbnds 'x) (mkbnds 'y) (mkbnds 'z))
        (apply #'values (nreverse vals))))
  (prebegin-z (4 3 2 5))
  (prebegin-y (4 3 2 5))
  (prebegin-x (4 3 2 5)) (4 3 2 5) (postbegin-x (4 3 2 5)) (4 5 2 5) (4 7 2 5) (preend-x (4 7 2 5)) (4 9 2 5) (postend-x (4 9 2 5))
  (postbegin-y (4 9 2 5))
  (prebegin-x (4 3 4 5)) (4 3 4 5) (postbegin-x (4 3 4 5)) (4 5 4 5) (4 7 4 5) (preend-x (4 7 4 5)) (4 9 4 5) (postend-x (4 9 4 5))
  (prebegin-x (4 3 6 5)) (4 3 6 5) (postbegin-x (4 3 6 5)) (4 5 6 5) (4 7 6 5) (preend-x (4 7 6 5)) (4 9 6 5) (postend-x (4 9 6 5))
  (preend-y (4 3 6 5))
  (prebegin-x (4 3 8 5)) (4 3 8 5) (postbegin-x (4 3 8 5)) (4 5 8 5) (4 7 8 5) (preend-x (4 7 8 5)) (4 9 8 5) (postend-x (4 9 8 5))
  (postend-y (4 9 8 5))
  (postbegin-z (4 9 8 5))
  (preend-z (4 3 2 5))
  (prebegin-y (4 3 2 7))
  (prebegin-x (4 3 2 7)) (4 3 2 7) (postbegin-x (4 3 2 7)) (4 5 2 7) (4 7 2 7) (preend-x (4 7 2 7)) (4 9 2 7) (postend-x (4 9 2 7))
  (postbegin-y (4 9 2 7))
  (prebegin-x (4 3 4 7)) (4 3 4 7) (postbegin-x (4 3 4 7)) (4 5 4 7) (4 7 4 7) (preend-x (4 7 4 7)) (4 9 4 7) (postend-x (4 9 4 7))
  (prebegin-x (4 3 6 7)) (4 3 6 7) (postbegin-x (4 3 6 7)) (4 5 6 7) (4 7 6 7) (preend-x (4 7 6 7)) (4 9 6 7) (postend-x (4 9 6 7))
  (preend-y (4 3 6 7))
  (prebegin-x (4 3 8 7)) (4 3 8 7) (postbegin-x (4 3 8 7)) (4 5 8 7) (4 7 8 7) (preend-x (4 7 8 7)) (4 9 8 7) (postend-x (4 9 8 7))
  (postend-y (4 9 8 7))
  (postend-z (4 9 8 7)))
