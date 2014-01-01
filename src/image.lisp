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

(defun temp-image (path)
  "Return a temporary image pathname relative to PATH"
  (pathname-as-directory (dir+file path "tmp")))

;;; TODO The node may be removed if it has no kids and one sets all
;;; its tiles to the default value.

(defun tmp2kid (node loc)
  "Rename the temp image to the kid of NODE at the location LOC.
Also remove empty nodes."
  ;; TODO Remove empty nodes
  (rename-directory (temp-image node)
                    (pathname-as-directory
                     (dir+file node (prin1-to-string (kid-num node loc))))))

;;; Use it for removing or kid converting
;;; Maybe scan-dtree in dtree.lisp?
(defun remove-dtree-if (dtree fn))

(defun write-props (node)
  "Write properties for NODE."
  (write-file (list (cons :offsets
                          ;; We do not use offsets here
                          (make-list *new-node-resolution*)))
              (dir+file node *node-properties-filename*)
              :supersede))

(defmacro with-tilevars (vars &body body)
  "Do BODY with tile variables names VARS.
The following variables a bound to the tile variables name: TODO add
var names"
  `(destructuring-bind (&optional
                        (tilev 'tile)
                        (tileresv 'tileres)
                        (tilelocv 'tileloc)
                        (bl1v 'bl1)
                        (bl2v 'bl2)) vars
     ,@body))

(defun mkfindlowfns (targetres vars node loc l1 l2 findform lowform)
  "TODO Doc"
  (let ((gtargetres (gensym)))
    (with-tilevars vars
      (flet ((mkfn (form)
               `#'(lambda (,node ,@args)
                    (let* ((,gtargetres ,targetres)
                           (,tileresv ,`(- ,gtargetres (locat-r ,loc)))
                           (,tilelocv (resol ,gtargetres ,loc))
                           (,bl1v ,l1)
                           (,bl2v ,l2))
                      (symbol-macrolet
                          ((,tilev ,`(node-tile ,node ,tileresv)))
                        ,form)))))
        (list (mkfn findform) (mkfn lowform))))))

(defmacro walk-image-box ((dtree loc1 loc2 &rest vars)
                          &body form1-and-form2)
  "Walk the box [L1; L2] in the image DTREE.
If the current tile's data available evaluate the first form of
FORM1-AND-FORM2.  Otherwise evaluate the second one.

The following variables available in scope of the forms.

Default variable name                  Bound to

      TILE             The current tile's data if it is available.
                       Otherwise nil.

      TILERES          The tile resolution.

      TILELOC          The tile location in the image.

      BL1, BL2         The box [L1; L2] clipped to the tile size."
  (destructuring-bind (form1 form2) form1-and-form2
    (with-gensyms (node loc l1 l2 gloc1)
      `(let ((,gloc1 ,loc1))
         (find-nodes-box dtree ,gloc1 l2
                         ,@(mkfindlowfns
                            (locat-r gloc1) vars node loc l1 l2
                            `(if (file-exists-p (tile-file ,node ,tileresv))
                                 ,form1
                                 ,form2)
                            `(progn
                               (create-nodes-box
                                (temp-image ,node) ,l1 ,l2
                                #'(lambda (,node ,loc ,l1 ,l2)
                                    (write-props ,node)
                                    ,(with-tilevars vars
                                       `(setq ,tileresv ?
                                              ,tilelocv ?
                                              ,bl1v ?
                                              ,bl2v ?))
                                    ,form2))
                               rm)))))))
