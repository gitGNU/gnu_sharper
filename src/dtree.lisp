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

(defmacro traverse-node (node res nodevar resvar kidargs &body res-low-forms)
  "Traverse the dtree from the root NODE to the resolution RES.

The dtree traversal is the same for many functions such as
`find-node', `create-node', `find-nodes-box' etc.  This macro helps to
make dtree traversal functions.  The algorithm consists of the
following steps:

1. Add the root NODE resolution to the resolution sum which is zero at
the beginning.

2. If the sum is equal or greater than the target resolution RES
evaluate the first form of the forms RES-LOW-FORMS.

3. Otherwise evaluate the rest of the forms RES-LOW-FORMS.

In scope of the forms two local macros are bound: `traverse-kid' and
`if-kid'.

`traverse-kid' kid &rest kidargs

Continue the traversal from the current node to its kid KID, i.e. do
the step 1 of the algorithm with the kid as the root and the current
resolution sum which is implicitly passed to the recursive call.  Also
pass to it other user-defined arguments KIDARGS.

`if-kid' loc then else

If the kid of the current node at the location LOC is present evaluate
the form THEN and bound the kid to the symbol IT, otherwise evaluate
the form ELSE."
  (with-gensyms (gtrav gres)
    (let* ((resform (car res-low-forms))
           (lowform `(progn ,@(cdr res-low-forms)))
           (travbody
            `(let ((,resvar (+ ,resvar (node-resolution ,nodevar))))
               (if (>= ,resvar ,gres)
                   ,resform
                   ,lowform))))
      `(macrolet ((if-kid (loc then &optional else)
                    `(aif (kid-at ,',nodevar ,loc)
                          ,then ,else))
                  (traverse-kid (kid ,@kidargs)
                    `(,',gtrav ,kid ,',resvar ,,@kidargs)))
         (let ((,gres ,res)
               (,nodevar ,node)
               (,resvar 0))
           (labels ((,gtrav (,nodevar ,resvar ,@kidargs)
                      ,travbody))
             ,travbody))))))

;;; FIXME There is a little mess with type of nodes. Sometimes
;;; `create-nodes' and `find-node' return a string, sometimes a
;;; pathname.

;;; TODO Replace WRITEFN with corresponding closure in ROOT.
(macrolet
    ((deftraverse (name args doc lowform &optional (nodeform 'node))
       `(defun ,(symbolicate name '-node) (node loc ,@args)
          ,doc
          (traverse-node ,nodeform (locat-r loc) curnode cures ()
            curnode
            (let ((curloc (resol cures loc)))
              (if-kid curloc
                      (traverse-kid it)
                      ,lowform))))))

  (deftraverse create (writefn)
    "Create the node at the location LOC in the dtree NODE.
Create parent nodes and the root NODE if they are not present.  If the
node at requested location is already exist do not recreate it
Also for its parents.

Make directories for each created node and call the function WRITEFN
which is responsible to create data for the node.  The function should
take the following arguments: the created node, the current location.
The current location is the requested location LOC but in case of
creation of parent nodes it has lower resolution (parent resolution).
If the root node is created the current location is NIL.

Return the last created node.  If there is no one return NIL."
    (traverse-kid
     (let ((kid (pathname
                 (format nil "~A~D/" curnode (kid-num curnode curloc)))))
       (ensure-directories-exist kid)
       (funcall writefn kid curloc)
       kid))
    (if (cl-fad:directory-exists-p node)
        node
        (progn (ensure-directories-exist node)
               (funcall writefn node nil)
               node)))

  (deftraverse find ()
      "Find the node at the location LOC in the dtree NODE.
Return two values: the found node and the requested location LOC.  If
there is no any node at requested resolution (locat-r LOC) return the
node at maximum available resolution and the location `resol''ed
\(scaled) to the resolution."
    (values curnode curloc)))

;;; FIXME The function works incorrectly with existing dtree.
;;; (progn (create-nodes-box "~/tmp/root/" (locat 2 0 0 0) (locat 2 1 1 0)
;;;                          #'(lambda (n l) (declare (ignore n l))) 2)
;;;        (create-nodes-box "~/tmp/root/" (locat 5 4 1 2) (locat 5 11 9 5)
;;;                          #'(lambda (n l) (declare (ignore n l))) 3))
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
                                 (create-node root l writefn))))))
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
