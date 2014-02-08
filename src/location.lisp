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

;;; Commentary:

;; Raster images can be represented by a regular grid.  All cells of
;; the grid have equal sides and the size of each side is 1.

;; A particular cell location in space can be defined by the list of
;; coordinates in the grid.

;; Because raster images can have different resolutions, that is the
;; size of a side, we should store resolution in the location to be
;; able to compare and scale images with different resolutions. Note
;; that we do not need to store resolution of each side because we
;; have the regular grid and it is sufficient to store the maximum
;; resolution to be able to compare images.  Also, for simplicity the
;; current implementation stores resolutions as power of two and the
;; minimum one is 1 (there is no sense to have images with one
;; element).

;;;; This file contains utilities for working with the locations.

;;; Code:

(in-package #:sharper)

;;; FIXME Can't inline `assert-resol' on SBCL.
;; (declaim (inline assert-resol))
(defun assert-resol (r)
  "Signal an error if the resolution R is less than 1."
  (assert (>= r 1) nil
          "Resolution R must be greater or equal to 1 ~
           but ~D was given" r))

(declaim (inline isize))
(defun isize (r)
  "Return the image size with the resolution R.
The image size is the size of each side of the image.

+----+
|    |
|    | isize => 4
|    |
|    |
+----+

Sizes of the sides are always equal.  The resolution R is not checked
for correct values."
  (expt 2 r))

(defun locat (r x &rest more)
  "Make a location with coordinates (X . MORE) and the resolution R."
  (assert-resol r)
  (let ((coords (list* x more))
        (l (isize r)))
   (mapc #'(lambda (x)
             (if (>= (abs x) l)
              (error "The value of some coordinate is ~D,~%~
                      which is greater or equal to the image size ~D."
                     x l)))
         coords)
   (cons r coords)))

(declaim (inline locat-r))
(defun locat-r (l)
  "Get resolution of the location L."
  (car l))

(declaim (inline coord))
(defun coord (l &optional n)
  "Get coords list of L or Nth coord when N is non-nil.
The first coord number is 0."
  (if n
      (nth (1+ n) l)
      (cdr l)))

(declaim (inline (setf coord)))
(defun (setf coord) (v l &optional n)
  "Set coords list of L or Nth coord when N is non-nil.
The first coord number is 0."
  (if n
      (setf (nth (1+ n) l) v)
      (setf (cdr l) (coord (apply #'locat (locat-r l) v)))))

(declaim (inline copy-locat))
(defun copy-locat (l)
  "Make copy of the location L."
  (copy-list l))

(declaim (inline zeroloc))
(defun zeroloc (r axnum)
  "Make zero location with resolution R and coords number AXNUM.
Zero location is a location with all zero coordinates."
  (apply #'locat r (make-list axnum :initial-element 0)))

;;; I don't like the name
(declaim (inline maxloc))
(defun maxloc (r axnum)
  "Make maximum location with resolution R and coords number AXNUM.
Maximum location is a location every coordinate of which is equal
to (1- (isize R))."
  (apply #'locat r
         (make-list axnum :initial-element (1- (isize r)))))

(defun resol (r l)
  "Scale the location L to the resolution R."
  (assert-resol r)
  (let ((n (isize (- (locat-r l) r))))
    (apply #'locat r (mapcar #'(lambda (x)
                                 (truncate (/ x n)))
                             (coord l)))))

(defun unfold (l)
  "Reduce the number of dimensions of the image to one.
The form

  (unfold (locat r x y z))

is equivalent to

  (+ (* z (isize r) (isize r)) (* y (isize r)) x)."
  (let ((m (isize (locat-r l)))
        (n (length (coord l))))
    (reduce #'+
            (reverse (coord l))
            :key #'(lambda (x)
                     (* x (expt m (decf n)))))))

(defun tile (r l)
  "Split the image to tiles with the resolution R.
Find the tile which contains the location L and return the location L
relative to its origin.

The form

  (tile 1 (locat 3 1 2 5))

is equivalent to

  ((rem 1 (isize 1))
   (rem 2 (isize 1))
   (rem 5 (isize 1)))."
  (assert-resol r)
  (let ((n (isize r)))
   (apply #'locat r (mapcar #'(lambda (x) (rem x n))
                            (coord l)))))

;;; TODO Resol the lesser locations res to the greatest one.  Make
;;; resol-to res-or-loc &rest locs
(defun walk-box-ranges (l1 l2 &optional bnds)
  "Make differences between coordinates of locations L1 and L2.
Also make boundaries functions lists for the function `walk-box'.  Use
boundaries function from the list BNDS if it is not NIL."
  (apply #'mapcar
         #'(lambda (a b &optional bnd)
             (let ((bnd (mapcar
                         #'(lambda (b)
                             (fcoerce b (l)
                               (declare (ignore l))))
                         (unless* bnd
                           (make-list 4 :initial-element nil)))))
               ;; FIXME The predicate should be supplied as
               ;; argument.
               (if (< a b)
                   (list a b bnd)
                   (list b a bnd))))
         (coord l1)
         (coord l2)
         (when bnds (list bnds))))

;;; TODO Add one step for each coord
;;; TODO Edit docstring
;;; TODO What value do the function return? Check all functions that
;;; use it.
;;; FIXME If there are no boundaries an error is occured
;;; TODO Making the list BOUNDARIES is ugly and confusing. See
;;; `walk-node-box'.
(defun walk-box (l1 l2 fn &optional (step 1) &rest boundaries)
  "Apply the function FN to each location in the box [L1; L2].
The box is specified by two corners: L1 and L2. Both corners are
included to the walking.

The function FN must take one argument: the current walking location.

The argument STEP may be a function or a number. In the first case,
the function STEP takes no arguments and return a number: who much to
walk from the current location to the next one.

The walking is performed in the following way:

  1. The walking begins from the location which is closest to the
  image origin and ends to the farthest one. Both locations may not to
  be equal to locations L1 and L2. They can be the other two corners
  of the box.

  2. The walking performs as the scanline rendering, row-by-row along
  the first axis.

At present, the walking performs only at resolution (locat-r L1)."
  (flet ((mkloc (coords) (apply #'locat (locat-r l1) coords)))
    (let* ((stepfn (fcoerce step))
           (callfn #'(lambda (coords)
                       (funcall fn (mkloc coords))))
           (ranges-bounds (walk-box-ranges l1 l2 boundaries))
           (corners (apply #'mapcar #'list ranges-bounds))
           (l1 (mkloc (car corners)))
           (l2 (mkloc (cadr corners))))
      (funcall
       (reduce #'(lambda (fn range-bound)
                   (destructuring-bind (begin
                                        end
                                        (prebegin
                                         postbegin
                                         preend
                                         postend))
                       range-bound
                     #'(lambda (coords)
                         (macrolet ((mkbnd (bname l)
                                      `(funcall
                                        ,bname
                                        (mkloc (append
                                                (butlast (coord ,l)
                                                         (1+ (length coords)))
                                                (cons n coords))))))
                           (for (n begin (<= n end) (+ n (funcall stepfn))
                                   :prebegin (mkbnd prebegin l1)
                                   :postbegin (mkbnd postbegin l2)
                                   :preend (mkbnd preend l1)
                                   :postend (mkbnd postend l2))
                             (funcall fn (cons n coords)))))))
               (cons callfn ranges-bounds))
       nil))))

(defun sort-box (l1 l2)
  "Rearrange the box [L1; L2] to [NEAREST; FARTHEST].
NEAREST and FARTHEST are the box corners that are nearest and farthest
to the origin respectively.

For example: L1 - the top-left corner, L2 - the bottom-right corner.

   ^  L1         F
   |   +--------+
   |   |        |
   |   |        |
   |   |        |
   |   +--------+
   |  N         L2
   |
   +--------------->
  O

Return two values: the nearest corner, the farthest corner."
  (let (a1 a2)
    (mapc #'(lambda (rb)
              (push (car rb) a1)
              (push (cadr rb) a2))
          (walk-box-ranges l1 l2))
    (values (apply #'locat (locat-r l1) (nreverse a1))
            (apply #'locat (locat-r l2) (nreverse a2)))))

(defun map-coords (fn l &rest more)
  "Apply the function FN to each coord of the location L.
Use MORE locations if they are supplied.

The function FN must take the number of arguments equal to the number
of supplied locations (L + MORE). If resolutions of locations is not
equal then locations are `resol'ed to the maximum one."
  (let* ((ls (cons l more))
         (r (reduce #'max ls :key #'locat-r)))
    (apply #'locat r
           (apply #'mapcar fn
                  (mapcar #'(lambda (l)
                              (coord (resol r l)))
                          ls)))))

(defmacro def-locat-op (name op)
  "Define the operation OP to be perfomed on locations.
Define the function NAME that applies the function `map-coords' with the
function OP to one or more locations."
  `(defun ,name (l &rest more)
     (apply #'map-coords #',op l more)))

(macrolet ((defops (&rest ops)
             `(progn
                ,@(mapcar #'(lambda (op)
                              `(def-locat-op
                                   ,(symbolicate 'locat op)
                                   ,op))
                          ops))))
  (defops + - * /))

(defun move (loc &rest deltas)
  "Move the location LOC."
  (map-coords #'(lambda (x)
                  (+ x (pop deltas)))
              loc))

(defun move* (loc &rest locs)
  "Move the location LOC using coords as deltas from each location in LOCS."
  (reduce #'(lambda (l d)
              (apply #'move l d))
          locs
          :key #'coord
          :initial-value loc))
