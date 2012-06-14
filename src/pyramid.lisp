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

;; Pyramid - a multiresolution image

;; This code is used for experimentation purposes and will be removed
;; soon.

;;; Code:

(in-package #:sharper)

(defun pyramid-resolution (sizes)
  "Get the pyramid resolution. SIZES - a list of arrays sizes. See
`load-pyramid'."
  (length sizes))

(defun make-pyramid-array (resolution dimensions &optional (initial-element 0))
  "Make an array."
  (make-array (expt (ilength resolution) dimensions)
              :initial-element initial-element))

(defun make-pyramid (&rest arrays)
  "Make a new pyramid. ARRAYS - a list with arrays for the new
pyramid."
  (copy-list arrays))

(defun load-pyramid (filename sizes)
  "Load the pyramid image from FILENAME. DIMENSIONS - the number of
dimensions of the pyramid. ELEMENT-BYTE-SIZE - the size of an element
in bytes."
  (with-open-file (f filename :direction :input :element-type 'unsigned-byte)
    (let* ((len (file-length f))
           (bytes (make-array len)))
      (read-sequence bytes f)
      (loop for sz in sizes
         with st = 0
         collect (subseq bytes st (+ st sz))
         do (incf st sz)))))

(defun save-pyramid (filename pyramid &optional (if-exists :supersede))
  "Save the PYR pyramid to the FILENAME file. The function returns the
list of arrays sizes."
  (with-open-file (f filename
                     :direction :output
                     :if-exists if-exists
                     :element-type 'unsigned-byte)
    (mapcar #'(lambda (array)
                (write-sequence array f)
                (length array))
            pyramid)))

(defun pyramid-array (pyramid resolution)
  "Get the array in the pyramid PYRAMID at resolution RESOLUTION."
  (nth resolution pyramid))

(defun (setf pyramid-array) (new-array pyramid resolution)
  "Set the array NEW-ARRAY to the pyramid PYRAMID at resolution
RESOLUTION."
  (setf (nth resolution pyramid) new-array))
