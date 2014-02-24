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

;;; The image is a wrapper on dtree.  It represents a multiresolution
;;; image as the whole without dtree nodes and file operations
;;; considerations.  However the image is represented as a set of
;;; tiles at particular resolution and you should be able to make your
;;; representation from them, e.g. merge tiles into one image.  Also,
;;; to modify the image you should extract a tile from your
;;; representation.

;;; The following figure demonstrates the idea.

;;; The dtree

;;;          **
;;;     ----****----
;;;    /    /  \    \
;;;  **   **    **   **
;;; **** ****  **** ****

;;; The image

;;;          **         resolutions 1, 2: 1 tile
;;;         ****
;;;     ** ** ** **     resolutions 3, 4: 4 tiles
;;; **** **** **** ****

;;; When you read the image at particular location you get a tile
;;; which cover the image area including the queried location.  For
;;; updating the location you have to update the entire tile.

;;; Tiles are byte vectors.  They are storied in the image as is.
;;; When reading you should convert byte vectors to your
;;; representation, e.g. arrays of color components RGB.  When writing
;;; your representation should be converted to a byte vector.

;;; The image is repesented as an infinite canvas, i.e. you can read
;;; or write data at arbitrary resolution.  Tiles which are not
;;; created have value NIL.  Tiles are created by writing to them.

;;; Code:

(in-package #:sharper)

(defparameter *new-node-resolution* 8
  "Resolution of newly created nodes.
At the moment this is the only way to change the resolution of the
dtree nodes which are should be created upon image write operations.")

;;; Very simple implementation: each tile in the node is stored as a
;;; separate file

(defun tile-file (node res)
  "Return the tile filename of NODE at the resolution RES."
  (format nil "~A~D" (dir+file node "tile") res))

(defun node-tile (node res)
  "Return the tile of NODE at the resolution RES."
  (when (file-exists-p (tile-file node res))
    (read-file-into-byte-vector (tile-file node res))))

(defun (setf node-tile) (tile node res)
  "Set the new tile TILE of NODE at the resolution RES."
  ;; TODO If TILE is nil delete its file
  (write-byte-vector-into-file tile
                               (tile-file node res)
                               :if-exists :supersede
                               :if-does-not-exist :create)
  tile)

(defun temp-image (dir)
  "Return a temporary image pathname relative to DIR.
If DIR does not exist return it.  In such a case, the temporary image
become a new dtree root."
  (if (directory-exists-p path)
      (pathname-as-directory (dir+file path "tmp"))
      path))

(defun write-props (node)
  "Write properties for NODE."
  (write-file (list (cons :offsets
                          ;; We do not use offsets here
                          (make-list *new-node-resolution*)))
              (dir+file node *node-properties-filename*)
              :supersede))

(defmacro with-tilevars (node loc parentloc k1 k2 &body body)
  "Do BODY with tile variables: TILE, TILERES, TILELOC, BL1, BL2."
  (with-gensyms (lres ploc gloc gnode)
    `(let* ((,gnode ,node)
            (,lres (locat-r ,loc))
            (,ploc ,parentloc)
            (tileres (- ,lres (locat-r ,ploc)))
            (,gloc (resol ,lres ,ploc))
            (bl1 (move* ,gloc k1))
            (bl2 (move* ,gloc k2)))
       (symbol-macrolet ((tile (node-tile ,gnode ,lres)))
         ,@body))))

(defmacro walk-image-box (dtree loc1 loc2
                          &body (form1 &optional (form2 form1)))
  "Walk the box [LOC1; LOC2] in the image DTREE.
If the current tile's data available evaluate FORM1.  Otherwise
evaluate FORM2.

The following variables available in scope of the forms.

Variable name          Bound to

   TILE          The current tile's data if it is available.
                 Otherwise nil.

   TILERES       The tile resolution.

   TILELOC       The tile location in the image.

   BL1, BL2      The box [LOC1; LOC2] clipped to the tile size.

TILE is generalized variable.  If `setf' it to the new value the value
is written as the tile's data.  If `setf' TILE to nil the tile's data
is deleted."
  ;; TODO If TILE is deleted check the other tiles and kids of its
  ;; node.  If the node has no tiles and kids delete it.
  (with-gensyms (gloc1 tmp node parentloc kl1 kl2 nodec kl1c rm)
    `(let ((,gloc1 ,loc1))
       (find-nodes-box
        ,dtree ,gloc1 ,loc2
        #'(lambda (,node ,parentloc ,kl1 ,kl2)
            (with-tilevars ,node ,gloc1 ,parentloc ,kl1 ,kl2
              (if (file-exists-p (tile-file ,node tileres))
                  ,form1
                  ,form2)))
        #'(lambda (,node ,parentloc ,kl1 ,kl2)
            (let* ((,tmp (temp-image ,node))
                   (,rm #'(lambda ()
                            (delete-directory-and-files ,tmp))))
              (create-nodes-box
               ,tmp ,kl1 ,kl2
               #'(lambda (,nodec ,parentloc ,kl1c ,kl2)
                   (write-props ,nodec)
                   (with-tilevars ,nodec ,loc1 ,parentloc ,kl1c ,kl2
                     (flet (((setf node-tile) (tile ,nodec tileres)
                              "Set tile and rename TMP to NODE's kid."
                             (setf ,rm #'(lambda ()
                                           ;; TODO Remove empty nodes
                                           (move-dtree ,tmp ,node ,kl1))
                                   (node-tile ,nodec tileres) tile)))
                       ,form2))))
              (funcall ,rm)))))))

;; Description of VARS for `walk-image-box' binding of which is not
;; implemented yet.
;;
;; VARS is a list of variables names which are available in scope of the
;; forms.  If some variable name is nil it has the default name.  If the
;; variable list is nil, all variables have default names.  The following
;; table describes the variables binding.
