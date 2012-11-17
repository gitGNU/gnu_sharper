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

;;; Code:

(defpackage #:sharper-test-system
  (:use #:cl #:asdf))

(in-package #:sharper-test-system)

(asdf:defsystem :sharper-test
  :serial t
  :depends-on (:rt :sharper)
  :components ((:file "package")
               (:file "utils")
               (:file "location")
               (:file "dtree")))

(defmethod perform ((o test-op) (c (eql (find-system :sharper-test))))
  (declare (ignorable o c))
  (funcall (find-symbol "DO-TESTS" "SHARPER-TEST")))
