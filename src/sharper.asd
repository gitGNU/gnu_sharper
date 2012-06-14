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

(in-package #:asdf)

(defsystem :sharper
  :serial t
  :depends-on (:alexandria :cl-fad)
  :in-order-to ((test-op (test-op :sharper-test)))
  :components ((:file "package")
               (:file "utils")
               (:file "location")
               (:file "pyramid")
               (:file "dtree")))
