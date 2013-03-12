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

(defpackage #:sharper
  (:use #:common-lisp #:cl-fad)
  (:import-from #:alexandria
                #:with-gensyms
                #:symbolicate)
  (:export
   ;; Utils
   #:it
   #:aif
   #:awhen
   #:mapa-b
   #:map0-n
   #:map1-n
   #:cons-if
   #:cons-if*
   #:unless*
   #:while
   #:for
   #:cat
   #:read-file
   #:write-file
   #:file-size
   #:iflet
   #:iflet*
   #:pathname-eq
   #:clamp-min
   #:clamp-max
   #:dir+file
   #:ceil
   ;; Location
   #:ilength
   #:locat
   #:locat-r
   #:locat-axes
   #:resol
   #:walk-box
   #:map-axes
   #:locat+
   #:locat-
   #:locat*
   #:locat/
   ;; Pyramid
   #:make-pyramid
   #:save-pyramid
   ;; Dtree
   #:*default-node-resolution*
   #:*node-properties-filename*
   #:*node-pyramid-filename*
   #:node-prop
   #:node-resolution
   #:dtree-resolution
   #:create-node
   #:find-node
   #:create-nodes-box
   #:find-nodes-box))
