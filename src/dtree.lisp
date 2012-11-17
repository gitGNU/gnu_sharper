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

;;; TODO Rename to node-res
;;; TODO The function is not used at the moment
(defun node-resolution (node)
  "Get the resolution of the node NODE."
  (pyramid-resolution (node-prop node :offsets)))

;;; TODO Rename to dtree-res
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

;;; TODO Generalize `create-nodes', `find-node' and `find-nodes-box'
;;; in the function `traverse-dtree'.

;;; FIXME There is a little mess with type of nodes. Sometimes
;;; `create-nodes' and `find-node' return a string, sometimes a
;;; pathname.

;;; Maybe return location of the node's origin like `create-nodes-box'
;;; does.
;;; TODO Take a function for node writing. In this case we do not need
;;; return two values, because the function will take the node being
;;; created, its resolution and location.
;;; The argument WRITEFN will be replaced by corresponding functional
;;; object in ROOT.
(defun create-nodes (root loc writefn)
  "Create nodes from the node ROOT to the location LOC.
Create nodes from the node ROOT i.e. from the location (locat 1 0 ...)
to the location LOC. Do not create nodes if they already exist. The
resolution of created nodes is NODE-RES.

Return the last created node which contains the location LOC and its
origin. See `node-origin'.

The function can be used to create a new directory tree."
  (labels
      ((wn (node loc)
         (let ((kid (pathname
                     (format nil "~A~D/" node (kid-num node loc)))))
           (ensure-directories-exist kid)
           (funcall writefn kid loc)))
       (cn (node res)
         (let ((r (+ res (node-resolution node))))
           (if (>= r (locat-r loc))
               node
               (aif (kid-at node (resol r loc))
                    (cn it r)
                    (cn (wn node (resol r loc)) r))))))
    (unless (directory-exists-p root)
      (funcall writefn root nil))
    (cn root 0)))

;;; TODO Return just the found node. It is sufficient to get its
;;; resolution and the location in it. But if we get a node at lower
;;; resolution?
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

;;; FIXME The function works incorrectly with existing dtree.
;;; (progn (create-nodes-box "~/tmp/root/" (locat 2 0 0 0) (locat 2 1 1 0) #'(lambda (n l) (declare (ignore n l))) 2)
;;;        (create-nodes-box "~/tmp/root/" (locat 5 4 1 2) (locat 5 11 9 5) #'(lambda (n l) (declare (ignore n l))) 3))
;;; This should make the dtree whith resolution 5 but 8 is created.
(defun create-nodes-box (root loc1 loc2 writefn)
  "Create nodes in the box [LOC1; LOC2] from the node ROOT.
Perform the box walking (see `walk-box') with the function
`create-nodes'. Apply the function FN to the created node and the
current location. The order of applying the function FN to created
nodes is undefined. The resolution of created nodes is NODE-RES."
  (symbol-macrolet
      ((set-step (setq step
                       (ilength (node-resolution
                                 (create-nodes root l writefn))))))
    (let* ((step)
           (current)
           (walk #'(lambda (l) set-step))
           (begin #'(lambda (l)
                      (declare (ignore l))
                      (setq current walk))))
      (setq current begin)
      set-step
      ;; TODO Move loc1 to tile origin.
      (walk-box loc1 loc2
                #'(lambda (loc)
                    (funcall current loc))
                #'(lambda () step)))))

;; (defmacro with-kids-locs ((kids-locs) &body body)
;;   "TODO Docstring"
;;   (let ((setters (gensym)))
;;     `(let (,@kids-locs
;;            ,setters)
;;        (flet ((mksetter (axis kidloc loc)
;;                 #'(lambda ()
;;                     (setf (locat-axis axis kidloc)
;;                           (locat-axis axis loc))))
;;               (proclocs (kidres)
;;                 (dolist (s ,setters)
;;                   (funcall s kidres))
;;                 (setq ,setters nil))))
;;        ,@body)))

;; (defun map-kids-box (node loc1 loc2 kid &optional nokid)
;;   "TODO Docstring
;; Return the list of `kid' and `nokid' results."
;;   (let ((r (locat-r loc1))
;;         (nr (node-resolution node))
;;         (kid (fcoerce kid (n l1 l2)
;;                       (list n l1 l2)))
;;         (nokid (fcoerce nokid (l) l))
;;         kl1 kl2 prx)
;;     (macrolet ((letfns ((&rest fns) &body body)
;;                  `(let ,fns body))
;;                (runfns ()
;;                  (progn ,@(loop for fn in fns
;;                              collect `(funcall fn (kr)))))
;;                mkbnds)
;;       (with-
;;        (apply #'walk-box (resol nr loc1) (resol nr loc2)
;;               #'(lambda (l)
;;                   (aif (kid-at node l)
;;                        (progn ...)
;;                        (funcall nokid (resol r l))))
;;               1
;;               (loop for i from 0 below (length (locat-axes loc1))
;;                  collect (let ((i i))
;;                            (list
;;                             #'(lambda (l) ; Prebegin
;;                                 (setq kidl1
;;                                       #'(lambda (nr)
;;                                           (setf
;;                                            (nthcar i
;;                                                    (locat-axes l))
;;                                            (resol nr loc)))))
;;                             #'(lambda (l) ; Postbegin
;;                                 (setq kidl1
;;                                       #'(lambda (nr)
;;                                           (setf (nthcar i l)))))
;;                             #'(lambda (l) ; Preend
;;                                 (setq kidl2
;;                                       #'(lambda (nr)
;;                                           (setf (nthcar i l)))))
;;                             nil))))))))

(defun find-nodes-walk-box (res loc1 loc2 fn)
  "TODO Docstring"
  (multiple-value-bind (loc1 loc2) (sort-box loc1 loc2)
    (let* ((l1r (locat-r loc1))
           (n (length (locat-axes loc1)))
           (kidloc1 (copy-locat loc1))
           (kidloc2 (maxloc l1r n)))
      (flet ((mkbnd (axis kidloc loc)
               "TODO Docstring"
               #'(lambda (l)
                   (declare (ignore l))
                   (setf (locat-axis kidloc axis)
                         (locat-axis loc axis)))))
        (apply #'walk-box (resol res loc1) (resol res loc2)
               #'(lambda (l)
                   (funcall fn l
                            (copy-locat kidloc1)
                            (copy-locat kidloc2)))
               1
               (loop for i from 0 below n
                  collect (let ((i i))
                            (list
                             (mkbnd i kidloc1 loc1) ; Prebegin
                             (mkbnd i kidloc1 (zeroloc l1r n)) ; Postbegin
                             (mkbnd i kidloc2 loc2) ; Preend
                             (mkbnd i kidloc2 (maxloc l1r n)))))))))) ; Postend

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
        ((fnb (node loc loc1 loc2)
           (let ((loc-r (locat-r loc))
                 (nr (node-resolution node)))
             (if (>= loc-r maxres)
                 (let ((loc (resol maxres loc)))
                   (funcall fn node
                            (tile (- maxres loc-r (- nr)) loc)
                            loc))
                 (find-nodes-walk-box
                  nr loc1 loc2
                  #'(lambda (nl l1 l2)
                      (let ((loc (locat+ loc
                                         (apply #'locat loc-r
                                                (locat-axes nl)))))
                        (aif (kid-at node nl)
                             (fnb it
                                  (resol (node-resolution it) loc)
                                  l1 l2)
                             (funcall fn node nl loc)))))))))
      (fnb root
           (tile-origin (node-resolution root) loc1)
           loc1 loc2))))

;;; TODO The function `optimize-dtree'
