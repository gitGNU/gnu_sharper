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
;; which represents the original image with several levels of details.
;; The highest level represents the image at maximum resolution,
;; i.e. the original image.  The previous level represents half-scaled
;; version of the image.  The previous of previous level is a
;; half-scaled version of the previous representation.  The first
;; level resolution is 1, two elements per image side.

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
                   ;; numbers
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
                 (last* (pathname-directory p)))
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
(defun kid-num (node-res loc)
  "The kid number at LOC of nodes with resolution NODE-RES.
`tile' the image with NODE-RES and `unfold' the result.

For example.  The image has resolution 3.  Nodes resolution is 2.  The
location (3 6 3).  Then, the node 1 has the target kid and the kid
location in the node 1 is (2 1 2).  The kid number in the node 1 is
\(unfold (locat 2 1 2)) => 9.

+----+----+
|0   |1.  |
|    | .  |
|    |.*  |
|    |    |
+----+----+
|2   |3   |
|    |    |
|    |    |
|    |    |
+----+----+
"
  (unfold (tile node-res loc)))

(declaim (inline kid-at))
(defun kid-at (node loc)
  "Get the kid of the node NODE at the location LOC.
Return NIL if NODE does not have the kid."
  (kid node (kid-num (node-resolution node) loc)))

(defun kid-pathname (parent loc)
  "Make a pathname of the kid at LOC using its PARENT pathname."
  (make-pathname
   :directory
   (nconc (pathname-directory parent)
          (list (prin1-to-string
                 (kid-num (node-resolution parent) loc))))))

(defun walk-node-box (res loc1 loc2 fn)
  "Walk the box [LOC1; LOC2] at resolution RES.

At each location make the box for the kid at the current location
[KIDLOC1; KIDLOC2].  The function assume the resolution of the kid is
LOC1-resolution - RES.

For example.  Let the node which we wanted to walk has resolution 2.
The target box is [(4 2 2); (4 9 11)].  Because we should walk at the
node resolution the walking box is [(2 0 0); (2 2 2)].  The node may
have a kid at each location.  But the box for the kid differs from the
target box.  The box for kids at boundaries is clipped.  The box for
kids inside the target box is [(kres 0 0); (kres kmax kmax)], where
kres is the kid resolution, kmax = (ilength kres) - 1.  Because we do
not know the resolution of the kid at current resolution (it is the
job of the callback function FN to check the kid) assume the maximum
possible for the target box resolution: LOC1-resolution - RES, 4 - 2 =
2.

+----+----+----+----+  RES = 2
|0   |1   |2   |3   |  LOC1, LOC2 = (4 2 2), (4 1 3)
|loc1|    |    |    |
|  * |....|..  |    |  Kids boxes
|  . |    | .  |    |  kid0  => [(2 2 2); (2 3 3)]
+----+----+----+----+  kid1  => [(2 0 2); (2 3 3)]
|4 . |5   |6.  |7   |  kid2  => [(2 0 2); (2 1 3)]
|  . |    | .  |    |  kid4  => [(2 2 0); (2 3 3)]
|  . |    | .  |    |  kid5  => [(2 0 0); (2 3 3)]
|  . |    | .  |    |  kid6  => [(2 0 0); (2 1 3)]
+----+----+----+----+  kid8  => [(2 2 0); (2 3 3)]
|8 . |9   |10  |11  |  kid9  => [(2 0 0); (2 3 3)]
|  . |    | .  |    |  kid10 => [(2 0 0); (2 1 3)]
|  . |    |loc2|    |
|  . |....|.*  |    |
+----+----+----+----+
|    |    |    |    |
|    |    |    |    |
|    |    |    |    |
|    |    |    |    |
+----+----+----+----+
"
  (multiple-value-bind (loc1 loc2) (sort-box loc1 loc2)
    (let* ((lr (- (locat-r loc1) res))
           (n (length (coord loc1)))
           (tile1 (tile lr loc1))
           (tile2 (tile lr loc2))
           (kidloc1 (copy-locat tile1))
           (kidloc2 (maxloc lr n)))
      (flet ((mkbnd (coord kidloc loc)
               "Make a boundary function for `walk-box'"
               #'(lambda (l)
                   (declare (ignore l))
                   (setf (coord kidloc coord)
                         (coord loc coord)))))
        (apply #'walk-box (resol res loc1) (resol res loc2)
               #'(lambda (l)
                   (funcall fn l
                            (copy-locat kidloc1)
                            (copy-locat kidloc2)))
               1
               (loop for i from 0 below n
                  collect (let ((i i))
                            (list
                             (mkbnd i kidloc1 tile1) ; Prebegin
                             (mkbnd i kidloc1 (zeroloc lr n)) ; Postbegin
                             (mkbnd i kidloc2 tile2) ; Preend
                             (mkbnd i kidloc2 (maxloc lr n)))))))))) ; Postend

(defmacro traverse-node (node res nodevar resvar kidargs
                         &body (resform . lowforms))
  "Traverse the dtree from the root NODE to the resolution RES.
The dtree traversal is the same for many functions such as
`find-node', `create-node', `find-nodes-box' etc.  This macro helps to
make dtree traversal functions.  The algorithm consists of the
following steps:

1. Add the root NODE resolution to the resolution sum which is zero at
the beginning.

2. If the sum is equal or greater than the target resolution RES
evaluate RESFORM.

3. Otherwise evaluate LOWFORMS.

In scope of the forms two local macros are bound: `traverse-kid' and
`if-kid'.

`traverse-kid' kid &rest kidargs

Continue the traversal from the current node to its KID, i.e. do the
step 1 of the algorithm with the kid as the root and the current
resolution sum which is implicitly passed to the recursive call.  Also
pass to it other user-defined arguments KIDARGS.

`if-kid' loc then else

If the kid of the current node at the location LOC is present evaluate
the form THEN and bound the kid to the symbol IT, otherwise evaluate
the form ELSE."
  (with-gensyms (gtrav gres)
    (let* ((lowform `(progn ,@lowforms))
           (travbody
            `(let ((,resvar (+ ,resvar (node-resolution ,nodevar))))
               (if (>= ,resvar ,gres)
                   ,resform
                   ,lowform))))
      `(macrolet ((if-kid (loc then &optional else)
                    `(aif (kid-at ,',nodevar ,loc)
                          ,then ,else))
                  (traverse-kid (kid ,@(mapcar #'car kidargs))
                    `(,',gtrav ,kid ,',resvar ,,@(mapcar #'car kidargs))))
         (let ((,gres ,res)
               (,nodevar ,node)
               (,resvar 0)
               ,@kidargs)
           (labels ((,gtrav (,nodevar ,resvar ,@(mapcar #'car kidargs))
                      ,travbody))
             ,travbody))))))

;;; FIXME There is a little mess with type of nodes.  Sometimes
;;; `create-nodes' and `find-node' return a string, sometimes a
;;; pathname.

(defmacro deftraverse (name (loc . funargs) doc kidargs
                       &body (noform resform lowform))
  "Define the function NAME which performs dtree traversal.
The first function parameter a dtree node is bound to the symbol NODE.
The rest of parameters are the location LOC and FUNARGS.  DOC is the
function documentation string.  KIDARGS, RESFORM and LOWFORM are
passed to `traverse-node' macro.  NOFORM is evaluated when there is no
NODE."
  `(defun ,name (node ,loc ,@funargs)
     ,doc
     (aif (if (directory-exists-p node)
              node
              ,noform)
          (traverse-node it (locat-r ,loc) curnode cures ,kidargs
            ,resform
            ,lowform))))

(defmacro deftraverse-node (name args doc noform resform lowform)
  "Define the function NAME for the dtree one location traversal.
The first and second parameters of the function, the dtree node and
the target location are bound to the symbol NODE and LOC respectively.
The rest of parameters are ARGS.

DOC is a documentation string for the function.

NOFORM is evaluated when there is no NODE.  RESFORM and LOWFORM are
passed to `traverse-node' macro.

The macro is used to define two functions: `create-node' and
`find-node'.

See also `deftraverse' and `deftraverse-box'."
  `(deftraverse ,(symbolicate name '-node) (loc ,@args) ,doc ()
     ,noform
     ,resform
     (let ((curloc (resol cures loc)))
       (if-kid curloc
               (traverse-kid it)
               ,lowform))))

(defmacro deftraverse-box (name args doc noform resform lowform)
  "Define the function NAME for the dtree box traversal.
The function parameters are the dtree node, the target box.  The
parameters are bound to symbols: NODE and LOC1, LOC2 respectively.
The additional parameters are ARGS.

DOC is a documentation string for the function.

The dtree box traversal algorithm.

1. Walk the current node with the target box and its resolution.
2. Check the kid at each location.
3. If the kid is created evaluate RESFORM.
4. Otherwise evaluate LOWFORM.

NOFORM is evaluated when there is no NODE.

The macro is used to define two functions: `create-nodes-box' and
`find-nodes-box'.

See also `walk-node-box', `deftraverse' and `deftraverse-node'."
  ;; TODO Consider shorter names create-box and find-box
  `(deftraverse ,(symbolicate name '-nodes-box) (loc1 loc2 ,@args) ,doc
       ((parentloc (zeroloc 1
                            (length (coord loc1))))
        (kl1 loc1)
        (kl2 loc2))
     ,noform
     ,resform
     (walk-node-box
      (node-resolution curnode) kl1 kl2
      #'(lambda (l kl1 kl2)
          (let ((curloc (move* (resol cures parentloc) l)))
            (if-kid l
                    (traverse-kid it curloc kl1 kl2)
                    ,lowform))))))

(macrolet ((defcreate (type doc &rest args)
             (let ((noform
                    '(progn (ensure-directories-exist node)
                      (funcall writefn node nil loc1 loc2)
                      node))
                   (lowform
                    `(traverse-kid
                      (let ((kid (kid-pathname curnode curloc)))
                        (ensure-directories-exist kid)
                        (funcall writefn kid curloc ,@args)
                        kid))))
               `(,(symbolicate 'deftraverse- type)
                  create
                  (writefn)
                  ,doc
                  ,noform
                  curnode
                  (,@lowform ,@args)))))

  (defcreate node
      "Create the node at the location LOC in the dtree NODE.
Create parent nodes and the root NODE if they are not present.  If the
node at requested location is already exist do not recreate it and its
parents.

Make directories for each created node and call the function WRITEFN
which is responsible to create data for the node.  The function should
take two arguments: the created node, the current location.  The
current location is the requested location LOC but in case of creation
of parent nodes it has lower resolution (parent resolution).  If the
root node is created the current location is NIL.

Return the last created node.  If there is no one return NIL.")

  (defcreate box
      "Create nodes in the box [LOC1; LOC2].
The dtree root is NODE.  If it does not exist create it.  Do not
recreate any already created nodes.  Use the function WRITEFN to
create nodes data.  The function WRITEFN should take the following
arguments: the found node, its parent location and the box clipped by
NODE (two locations).

See also the macro `deftraverse-box'."
    curloc kl1 kl2))

(deftraverse-node find ()
  "Find the node at the location LOC in the dtree NODE.
Return two values: the found node and the requested location LOC.  If
there is no any node at requested resolution (locat-r LOC) return the
node at maximum available resolution and the location `resol''ed
\(scaled) to the resolution."
  nil
  (values curnode curloc)
  (values curnode curloc))

;; TODO The parameter FINDFN should be optional.  If it is nil then
;; the function `find-nodes-box' collect and return a list of found
;; nodes.
(deftraverse-box find (findfn &optional (lowfn findfn) (nofn lowfn))
  "Find nodes in the box [LOC1; LOC2].
The dtree root is NODE.  At each found node call the function FINDFN.
If the target resolution is not available call the optional function
LOWFN.  Default value of LOWFN is equal to FINDFN.  The functions
should take the following arguments: the found node, its parent
location and the box clipped by NODE (two locations).

See also the macro `deftraverse-box'."
  (progn (funcall nofn) nil)
  (funcall findfn curnode
           (unless
               (pathname= node curnode) parentloc)
           kl1 kl2)
  (funcall lowfn curnode curloc kl1 kl2))

(defun move-dtree (dtree parent &optional loc)
  "Move DTREE to PARENT and make it as PARENT's kid at LOC.
if LOC is nil do not rename DTREE.  Return the kid if LOC is non-nil,
otherwise the new pathname of DTREE."
  (if (pathname= dtree parent)
      dtree
      (rename-directory
       dtree
       (if loc
           (kid-pathname parent loc)
           (make-pathname :directory
                          (nconc (pathname-directory parent)
                                 (list dtree)))))))
