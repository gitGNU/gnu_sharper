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

;; The multiresolution raster image or pyramid is a set of images
;; which represents the original image with several levels of
;; details. The highest level represents the image at maximum
;; resolution, i.e. the original image. The previous level
;; represents half-scaled version of the image. The previous of
;; previous level is a half-scaled version of the previous
;; representation. The first level resolution is 1, two elements per
;; image side.

;; The directory tree decomposes the image to smaller size pyramids.

;; The original image

;;        **
;;       ****
;;     ********
;; ****************

;; The decomposed image

;;          **        <- the root directory
;;     ----****----
;;    /    /  \    \
;;  **   **    **   **   <- kids of the root directory
;; **** ****  **** ****

;;; Code:

(in-package #:sharper)

;;; TODO All functions that take a directory pathname should apply
;;; `pathname-as-directory' on it. It is more convenient to give them
;;; the pathname as directory regardless of the last slash.

(defparameter *default-node-resolution* 4
  "The new node has the pyramid with this resolution.")

(defparameter *node-properties-filename* "prop"
  "The node properties filename.
The properties file is a file which is located in the node directory
and contains the list of offsets from the pyramid file beginning to
arrays in the pyramid. The list in the node properties file is defined
in the following way:

  (:offsets <offset-in-bytes-resolution1>
            <offset-in-bytes-resolution2>
            ...
            <offset-in-bytes-resolutionN>).")

(defparameter *node-pyramid-filename* "pyramid"
  "The filename of the pyramid in the node directory.")

(defun node-prop (node &optional key)
  "Get a property of the NODE by the key KEY.
See `*node-properties-filename*'. If key is NIL return the full node
property list."
  (let ((props (read-file (dir+file node *node-properties-filename*))))
    (if key
        (cdr (assoc key props))
        props)))

(defun kids (node)
  "Get the kids list of the node NODE."
  (remove-if #'(lambda (p)
                 (flet ((s= (p1 p2)
                          (aif (pathname-name p1)
                               (string= (namestring it) p2))))
                   ;; FIXME Remove all but directories with names as
                   ;; numbers that less than the maximum number of
                   ;; kids.
                   (or (s= p *node-properties-filename*)
                       (s= p *node-pyramid-filename*))))
             ;; TODO Use `cl-fad:walk-directory'.
             (directory (dir+file node "*/"))))

(defun kid (node num)
  "Get the kid number NUM of the node NODE.
Return NIL if the node NODE does not have the kid."
  (find (princ-to-string num)
        (kids node)
        :key #'(lambda (p)
                 ;; TODO We must handle symlinks!
                 (last1 (pathname-directory p)))
        :test #'string=))

(defun node-resolution (node)
  "Get the resolution of the node NODE."
  (pyramid-resolution (node-prop node :offsets)))

(defun dtree-resolution (root)
  "Calculate resolution of the dtree with the root node ROOT.
The dtree resolution is the resolution of the image decomposed by the
dtree."
  (+ (node-resolution root)
     (aif (kids root)
          (apply #'max (mapcar #'dtree-resolution it))
          0)))

(declaim (inline kid-num))
(defun kid-num (node loc)
  "Calculate the kid number of the node NODE at the location LOC.
The function does not check presence of the kid. It calculates what
the kid number should be at the location LOC. Note that the location
LOC is relative to the node."
  (unfold (tile (node-resolution node) loc)))

(declaim (inline kid-at))
(defun kid-at (node loc)
  "Get the kid of the node NODE at the location LOC.
Return NIL if NODE does not have the kid. The location LOC is relative
to the node."
  (kid node (kid-num node loc)))

(defun node-origin (loc &optional (node-res *default-node-resolution*))
  "Return the origin the node which is specified by the location LOC.
Return the tile origin with the resolution of the tile that is
multiple to the node resolution NODE-RES. See the function
`tile-origin'. "
  (let ((loc (resol (ceil (locat-r loc) node-res) loc)))
    (tile-origin node-res loc)))

(defun write-node (node pyramid)
  "Write the pyramid PYRAMID to the node directory NODE.
If the node does not exist create it otherwise rewrite it."
  (ensure-directories-exist node)
  (write-file
   (list (cons :offsets
               (save-pyramid (dir+file node *node-pyramid-filename*)
                             pyramid)))
   (dir+file node *node-properties-filename*)
   :supersede)
  node)

;;; TODO Generalize `create-nodes', `find-node' and `find-nodes-box'
;;; in the function `traverse-dtree'.

;;; FIXME There is a little mess with type of nodes. Sometimes
;;; `create-nodes' and `find-node' return a string, sometimes a
;;; pathname.

;;; Maybe return location of the node's origin like `create-nodes-box'
;;; does.
(defun create-nodes (root loc &optional (node-res *default-node-resolution*))
  "Create nodes from the node ROOT to the location LOC.
Create nodes from the node ROOT i.e. from the location (locat 1 0 ...)
to the location LOC. Do not create nodes if they already exist. The
resolution of created nodes is NODE-RES.

Return the last created node which contains the location LOC and its
origin. See `node-origin'.

The function can be used to create a new directory tree."
  (labels
      ((mkpyr ()
         (apply #'make-pyramid
                (map1-n #'(lambda (r)
                            (make-pyramid-array
                             r
                             (length (locat-axes loc))))
                        node-res)))
       (wn (node loc)
         (write-node (format nil "~A~D/" node (kid-num node loc))
                     (mkpyr)))
       (cn (node res)
         (let ((r (+ res (node-resolution node))))
           (if (>= r (locat-r loc))
               (values node (tile (- (locat-r loc) res) loc))
               (aif (kid-at node (resol r loc))
                    (cn it r)
                    (cn (wn node (resol r loc)) r))))))
    (unless (directory-exists-p root)
      (write-node root (mkpyr)))
    (cn root 0)))

(defun find-node (root loc)
  "Find a node in the dtree ROOT at the location LOC.
Traverse the dtree from the node ROOT, i.e. from the location
\(locat 1 0 ...) to the location LOC. Return the node which contains
the location LOC. Return the location L relative to it as the second
value, i.e. tile the image with the node resolution and the location
L. See the function `tile'.

If there is no node at the location L return the node at maximum
available resolution."
  (labels
      ((fd (node res)
         (let* ((noder (node-resolution node))
                (r (+ res noder)))
           (if (>= r (locat-r loc))
               (values node (- (locat-r loc) res) loc)
               (aif (kid-at node (resol r loc))
                    (fd it r)
                    (values node noder (resol r loc)))))))
    (multiple-value-bind (node noder found-loc) (fd root 0)
      (values node
              (tile noder found-loc)
              found-loc))))

(defun create-nodes-box (root loc1 loc2 fn
                         &optional (node-res *default-node-resolution*))
  "Create nodes in the box [LOC1; LOC2] from the node ROOT.
Perform the box walking (see `walk-box') with the function
`create-nodes'. Apply the function FN to the created node and the
current location. The order of applying the function FN to created
nodes is undefined. The resolution of created nodes is NODE-RES."
  (let ((nodes)
        (loc1 (node-origin loc1 node-res))
        (loc2 (node-origin loc2 node-res)))
    (walk-box loc1 loc2
              #'(lambda (loc)
                  (let ((n (create-nodes root loc node-res)))
                    (unless (pathname-eq n root)
                      (push (cons n loc) nodes))
                    (funcall fn n loc)))
              (ilength node-res))
    (while nodes
      (destructuring-bind (n . loc) (pop nodes)
        (let ((up (dirup n)))
          (unless (or (pathname-eq n root)
                      (member up nodes :test #'pathname-eq :key #'car))
            (let ((loc (node-origin
                        (resol (- (locat-r loc) node-res) loc) node-res)))
              (setq nodes (nconc nodes (list (cons up loc))))
              (funcall fn up loc))))))))

(defun find-nodes-box (root loc1 loc2 fn)
  "Find nodes for the box [L1; L2] in the dtree ROOT.
Perform the box walking (see `walk-box') in the dtree ROOT. Apply the
function FN to each found node, the `tile' of the current location and
the current location. The order of applying the function FN to found
nodes is undefined.

Please note! The current implementation is incomplete. the box [L1;
L2] must have the size of entire image, i.e. L1 => (locat R 0 0...);
L2 => (locat R (ilength R) (ilength R)...)."
  (let ((maxres (locat-r loc1)))
    (labels
        ((fnb (node noder loc l1 l2)
           (let ((loc-r (locat-r loc)))
             (if (>= loc-r maxres)
                 (let ((loc (resol maxres loc)))
                   (funcall fn node
                            (tile (- maxres loc-r (- noder)) loc) loc))
                 (walk-box
                  l1 l2
                  #'(lambda (l)
                      (let ((loc (locat+ loc
                                         (apply #'locat loc-r
                                                (locat-axes l)))))
                        (aif (kid-at node l)
                             (let ((nr (node-resolution it)))
                               (fnb it nr (resol (+ loc-r nr) loc) l1 l2))
                             (funcall fn node l loc)))))))))
      (let* ((nr (node-resolution root))
             (loc1 (resol nr loc1)))
        (fnb root nr (tile-origin nr loc1) loc1 (resol nr loc2))))))

;;; TODO The function `optimize-dtree'
