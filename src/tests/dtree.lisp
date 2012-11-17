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

(in-package #:sharper-test)

(defun namestring-car (lst)
  (apply #'values (namestring (car lst)) (cdr lst)))

(defun mkpyramid (res dim)
  "Make a new pyramid."
  (apply #'make-pyramid
         (map1-n #'(lambda (r)
                     (sharper::make-pyramid-array r dim))
                 res)))

(defun write-node (node res dim)
  "Write the pyramid PYRAMID to the node directory NODE.
Rewrite data of the node NODE."
  (write-file
   (list (cons :offsets
               (save-pyramid (dir+file node *node-pyramid-filename*)
                             (mkpyramid res dim))))
   (dir+file node *node-properties-filename*)
   :supersede)
  node)

(defmacro with-root-file (file &body body)
  "Remove the dtree \"/tmp/root/\" and the file FILE after execution
of forms BODY."
  `(unwind-protect
        (progn ,@body)
     (cl-fad:delete-directory-and-files "/tmp/root/")
     ,(when file `(delete-file ,file))))

(defmacro with-root (&body body)
  `(with-root-file nil ,@body))

;;; FIXME: The function `asdf:run-shell-command' is deprecated!
(declaim (inline shell-command))
(defun shell-command (control-string &rest args)
  "The function `asdf:run-shell-command' returns implementation
dependent values. E.g. in CLISP `ext:run-shell-command' returns NIL on
success but in SBCL `sb-ext:process-exit-code' returns 0."
  (let ((r (apply #'asdf:run-shell-command control-string args)))
    #+clisp (unless r (setq r 0))
    r))

(defun find-node-test (location)
  "Make a test tree and call `find-node' with LOCATION."
  (with-root
    (ensure-directories-exist "/tmp/root/801/")
    (ensure-directories-exist "/tmp/root/1074/22/")
    (write-node "/tmp/root/" 4 3)
    (write-node "/tmp/root/801/" 3 3)
    (write-node "/tmp/root/1074/" 2 3)
    (write-node "/tmp/root/1074/22/" 2 3)
    (namestring-car
     (multiple-value-list (sharper::find-node "/tmp/root/" location)))))

(deftest find-node-test1
    (find-node-test (locat 4 1 2 3))
  "/tmp/root/"
  (4 1 2 3)
  (4 1 2 3))

(deftest find-node-test2
    (find-node-test (locat 2 1 2 3))
  "/tmp/root/"
  (2 1 2 3)
  (2 1 2 3))

(deftest find-node-test3
    (find-node-test (locat 5 3 5 6))
  "/tmp/root/801/"
  (1 1 1 0)
  (5 3 5 6))

(deftest find-node-test4
    (find-node-test (locat 7 10 17 25))
  "/tmp/root/801/"
  (3 2 1 1)
  (7 10 17 25))

(deftest find-node-test5
    (find-node-test (locat 7 32 4 127))
  "/tmp/root/"
  (4 4 0 15)
  (4 4 0 15))

(deftest find-node-test6
    (find-node-test (locat 9 42 69 100))
  "/tmp/root/801/"
  (3 2 1 1)
  (7 10 17 25))

(deftest find-node-test7
    (find-node-test (locat 7 21 26 35))
  "/tmp/root/1074/22/"
  (1 1 0 1)
  (7 21 26 35))

;;; TODO Add comparing of the pyramid content for tests 5 and 6.
(defun create-nodes-test
    (location &optional (node-res sharper:*default-node-resolution*))
  "Make a test tree and call `create-nodes' with LOCATION."
  (with-root-file (dir+file "/tmp/" sharper::*node-pyramid-filename*)
    (ensure-directories-exist "/tmp/root/818/")
    (write-node "/tmp/root/" 4 3)
    (write-node "/tmp/root/818/" 3 3)
    (shell-command "head -c 584 /dev/urandom >/tmp/~A && ~
                                  cp /tmp/~A /tmp/root/818/"
                   sharper::*node-pyramid-filename*
                   sharper::*node-pyramid-filename*)
    (let* ((l (multiple-value-list
               (sharper::create-nodes "/tmp/root/" location node-res)))
           (len (with-open-file
                    (f (dir+file (car l) sharper::*node-pyramid-filename*))
                  (file-length f))))
      (namestring-car
       (append l
               (list len
                     (shell-command
                      "cmp -s /tmp/~A /tmp/root/818/~A"
                      sharper::*node-pyramid-filename*
                      sharper::*node-pyramid-filename*)))))))

(deftest create-nodes-test1
    (create-nodes-test (locat 3 7 5 4))
  "/tmp/root/"
  (3 7 5 4)
  4680
  0)

(deftest create-nodes-test2
    (create-nodes-test (locat 7 10 17 25))
  "/tmp/root/801/"
  (3 2 1 1)
  4680
  0)

(deftest create-nodes-test3
    (create-nodes-test (locat 7 10 17 25) 3)
  "/tmp/root/801/"
  (3 2 1 1)
  584
  0)

(deftest create-nodes-test4
    (create-nodes-test (locat 7 21 26 35) 2)
  "/tmp/root/1074/22/"
  (1 1 0 1)
  72
  0)

;;; `create-nodes' works as `find-node'
(deftest create-nodes-test5
    (create-nodes-test (locat 7 21 26 31))
  "/tmp/root/818/"
  (3 5 2 7)
  584
  0)

;;; `create-nodes' does not create the node "/tmp/root/818/", but
;;; creates the kid "/tmp/root/818/469/"
(deftest create-nodes-test6
    (create-nodes-test (locat 8 42 53 63))
  "/tmp/root/818/469/"
  (1 0 1 1)
  4680
  0)

;;; If there is no the root, create it.
(deftest create-nodes-test7
    (with-root
      (sharper::create-nodes "/tmp/root/" (locat 1 0 1)))
  "/tmp/root/"
  (1 0 1))

(defun create-nodes-box-test (loc1 loc2 &optional
                              (node-res sharper:*default-node-resolution*))
  "Make a test tree and call `create-nodes-box' with corners of the
box: LOC1 and LOC2."
  (with-root
    (let (nodes)
      (sharper:create-nodes-box "/tmp/root/"
                                loc1 loc2
                                #'(lambda (n l)
                                    (push (cons (namestring n) l) nodes))
                                node-res)
      (apply #'values (nreverse nodes)))))

(deftest create-nodes-box-test1
    (create-nodes-box-test (locat 1 0 0) (locat 1 0 0))
  ("/tmp/root/" 4 0 0))

(deftest create-nodes-box-test2
    (create-nodes-box-test (locat 4 10 2) (locat 4 12 4))
  ("/tmp/root/" 4 0 0))

(deftest create-nodes-box-test3
    (create-nodes-box-test (locat 2 1 2) (locat 2 3 0) 2)
  ("/tmp/root/" 2 0 0))

(deftest create-nodes-box-test4
    (create-nodes-box-test (locat 5 1 2) (locat 5 16 5))
  ("/tmp/root/16/" 8   0 16)
  ("/tmp/root/17/" 8  16 16)
  ("/tmp/root/18/" 8  32 16)
  ("/tmp/root/19/" 8  48 16)
  ("/tmp/root/20/" 8  64 16)
  ("/tmp/root/21/" 8  80 16)
  ("/tmp/root/22/" 8  96 16)
  ("/tmp/root/23/" 8 112 16)
  ("/tmp/root/24/" 8 128 16)
  ("/tmp/root/32/" 8   0 32)
  ("/tmp/root/33/" 8  16 32)
  ("/tmp/root/34/" 8  32 32)
  ("/tmp/root/35/" 8  48 32)
  ("/tmp/root/36/" 8  64 32)
  ("/tmp/root/37/" 8  80 32)
  ("/tmp/root/38/" 8  96 32)
  ("/tmp/root/39/" 8 112 32)
  ("/tmp/root/40/" 8 128 32)
  ("/tmp/root/"    4 0 0))

(deftest create-nodes-box-test5
    (create-nodes-box-test (locat 9 430) (locat 9 477) 3)
  ("/tmp/root/6/5/" 9 424)
  ("/tmp/root/6/6/" 9 432)
  ("/tmp/root/6/7/" 9 440)
  ("/tmp/root/7/0/" 9 448)
  ("/tmp/root/7/1/" 9 456)
  ("/tmp/root/7/2/" 9 464)
  ("/tmp/root/7/3/" 9 472)
  ("/tmp/root/7/"   6 56)
  ("/tmp/root/6/"   6 48)
  ("/tmp/root/"     3 0))

;;; TODO Add test when dtree is already created or partially
;;; created. Because with node creation we always have nodes at
;;; resolution equal or great to the requested resolution
;;; `create-nodes-box' must walk at maximum available resolution.


(defun find-nodes-walk-box-test (res loc1 loc2)
  "TODO Docstring"
  (let (locs)
    (sharper::find-nodes-walk-box
     res loc1 loc2
     #'(lambda (l l1 l2)
         (push (list l l1 l2) locs)))
    (apply #'values (nreverse locs))))

(deftest find-nodes-walk-box-test1
    (find-nodes-walk-box-test 4 (locat 8 65 16 3) (locat 8 20 50 37))
  ;; prebz
  ;; preby
  ;; prebx
  ((4 1 1 0) (8 20 16 3) (8 255 255 255))
  ((4 2 1 0) (8 0 16 3) (8 255 255 255))
  ((4 3 1 0) (8 0 16 3) (8 255 255 255))
  ;; preex
  ((4 4 1 0) (8 0 16 3) (8 65 255 255))
  ;; prebx
  ((4 1 2 0) (8 20 0 3) (8 255 255 255))
  ((4 2 2 0) (8 0 0 3) (8 255 255 255))
  ((4 3 2 0) (8 0 0 3) (8 255 255 255))
  ;; preex
  ((4 4 2 0) (8 0 0 3) (8 65 255 255))
  ;; preey
  ;; prebx
  ((4 1 3 0) (8 20 0 3) (8 255 50 255))
  ((4 2 3 0) (8 0 0 3) (8 255 50 255))
  ((4 3 3 0) (8 0 0 3) (8 255 50 255))
  ;; preex
  ((4 4 3 0) (8 0 0 3) (8 65 50 255))
  ;; preby
  ;; prebx
  ((4 1 1 1) (8 20 16 0) (8 255 255 255))
  ((4 2 1 1) (8 0 16 0) (8 255 255 255))
  ((4 3 1 1) (8 0 16 0) (8 255 255 255))
  ;; preex
  ((4 4 1 1) (8 0 16 0) (8 65 255 255))
  ;; prebx
  ((4 1 2 1) (8 20 0 0) (8 255 255 255))
  ((4 2 2 1) (8 0 0 0) (8 255 255 255))
  ((4 3 2 1) (8 0 0 0) (8 255 255 255))
  ;; preex
  ((4 4 2 1) (8 0 0 0) (8 65 255 255))
  ;; preey
  ;; prebx
  ((4 1 3 1) (8 20 0 0) (8 255 50 255))
  ((4 2 3 1) (8 0 0 0) (8 255 50 255))
  ((4 3 3 1) (8 0 0 0) (8 255 50 255))
  ;; preex
  ((4 4 3 1) (8 0 0 0) (8 65 50 255))
  ;; preez
  ;; preby
  ;; prebx
  ((4 1 1 2) (8 20 16 0) (8 255 255 37))
  ((4 2 1 2) (8 0 16 0) (8 255 255 37))
  ((4 3 1 2) (8 0 16 0) (8 255 255 37))
  ;; preex
  ((4 4 1 2) (8 0 16 0) (8 65 255 37))
  ;; prebx
  ((4 1 2 2) (8 20 0 0) (8 255 255 37))
  ((4 2 2 2) (8 0 0 0) (8 255 255 37))
  ((4 3 2 2) (8 0 0 0) (8 255 255 37))
  ;; preex
  ((4 4 2 2) (8 0 0 0) (8 65 255 37))
  ;; preey
  ;; prebx
  ((4 1 3 2) (8 20 0 0) (8 255 50 37))
  ((4 2 3 2) (8 0 0 0) (8 255 50 37))
  ((4 3 3 2) (8 0 0 0) (8 255 50 37))
  ;; preex
  ((4 4 3 2) (8 0 0 0) (8 65 50 37)))

(deftest find-nodes-walk-box-test2
    (find-nodes-walk-box-test 3 (locat 7 41 26 74) (locat 7 46 25 72))
  ((3 2 1 4) (7 41 25 72) (7 46 26 74)))

(defun find-nodes-box-test (loc1 loc2)
  "Make a test tree for `find-nodes-box'."
  (with-root
    (create-nodes-box "/tmp/root/" (locat 2 0 0 0) (locat 2 1 1 0)
                      #'(lambda (n l) (declare (ignore n l))) 2)
    (create-nodes-box "/tmp/root/" (locat 5 4 1 2) (locat 5 11 9 5)
                      #'(lambda (n l) (declare (ignore n l))) 3)
    (create-nodes-box "/tmp/root/" (locat 7 18 7 10) (locat 7 45 37 22)
                      #'(lambda (n l) (declare (ignore n l))) 2)
    (cl-fad:delete-directory-and-files "/tmp/root/4/")
    (cl-fad:delete-directory-and-files "/tmp/root/7/")
    (cl-fad:delete-directory-and-files "/tmp/root/20/")
    (cl-fad:delete-directory-and-files "/tmp/root/38/")
    (let (res)
      (find-nodes-box "/tmp/root/" loc1 loc2
                      #'(lambda (n tl l)
                          (push (list (namestring n)
                                      ;; At the present time, only the
                                      ;; tile resolution is used.
                                      (locat- tl tl)
                                      l) res)))
      (apply #'values (nreverse res)))))

(deftest find-nodes-box-test1
    (find-nodes-box-test-fn1 (locat 8 50 0) (locat 8 150 30))
  ("/tmp/root/3/" (4 0 0) (8 48 0))
  ("/tmp/root/" (4 0 0) (4 4 0))
  ("/tmp/root/5/" (4 0 0) (8 80 0))
  ("/tmp/root/6/" (4 0 0) (8 96 0))
  ("/tmp/root/" (4 0 0) (4 7 0))
  ("/tmp/root/8/" (4 0 0) (8 128 0))
  ("/tmp/root/9/" (4 0 0) (8 144 0))
  ("/tmp/root/19/" (4 0 0) (8 48 16))
  ("/tmp/root/" (4 0 0) (4 4 1))
  ("/tmp/root/21/" (4 0 0) (8 80  16))
  ("/tmp/root/22/" (4 0 0) (8 96  16))
  ("/tmp/root/23/" (4 0 0) (8 112 16))
  ("/tmp/root/24/" (4 0 0) (8 128 16))
  ("/tmp/root/25/" (4 0 0) (8 144 16)))

(deftest find-nodes-box-test2
    (find-nodes-box-test-fn1 (locat 5 6 0) (locat 5 18 3))
  ("/tmp/root/3/" (1 0 0) (5 6 0))
  ("/tmp/root/" (4 0 0) (4 4 0))
  ("/tmp/root/5/" (1 0 0) (5 10 0))
  ("/tmp/root/6/" (1 0 0) (5 12 0))
  ("/tmp/root/" (4 0 0) (4 7 0))
  ("/tmp/root/8/" (1 0 0) (5 16 0))
  ("/tmp/root/9/" (1 0 0) (5 18 0))
  ("/tmp/root/19/" (1 0 0) (5 6 2))
  ("/tmp/root/" (4 0 0) (4 4 1))
  ("/tmp/root/21/" (1 0 0) (5 10 2))
  ("/tmp/root/22/" (1 0 0) (5 12 2))
  ("/tmp/root/23/" (1 0 0) (5 14 2))
  ("/tmp/root/24/" (1 0 0) (5 16 2))
  ("/tmp/root/25/" (1 0 0) (5 18 2)))

(deftest find-nodes-box-test3
    (find-nodes-box-test-fn1 (locat 7 33 3) (locat 7 33 42))
  ("/tmp/root/" (4 0 0) (4 4 0))
  ("/tmp/root/" (4 0 0) (4 4 1))
  ("/tmp/root/36/" (3 0 0) (7 32 16))
  ("/tmp/root/" (4 0 0) (4 4 3))
  ("/tmp/root/" (4 0 0) (4 4 4))
  ("/tmp/root/" (4 0 0) (4 4 5)))

(deftest find-nodes-box-test4
    (find-nodes-box-test-fn1 (locat 6 29 1) (locat 6 30 9))
  ("/tmp/root/" (4 0 0) (4 7 0))
  ("/tmp/root/23/" (2 0 0) (6 28 4))
  ("/tmp/root/39/" (2 0 0) (6 28 8)))

(deftest find-nodes-box-test5
    (find-nodes-box-test-fn1 (locat 8 100 1) (locat 8 102 49))
  ("/tmp/root/6/" (4 0 0) (8 96 0))
  ("/tmp/root/22/" (4 0 0) (8 96 16))
  ("/tmp/root/" (4 0 0) (4 6 2))
  ("/tmp/root/" (4 0 0) (4 6 3)))

;; (locat 5 3 5 6)
;; root
;; (**)
;; (****)
;; (********)
;; (*xyz************)
;;  | \
;;  |  \   nodes
;; (**) (*x) (*y) (z*) (************************)
;;       r=5  r=5  r=5
;;       x=3  y=5  z=6

;; (locat 7 10 17 25)
;; root
;; (**)
;; (****)
;; (********)
;; (*xyz************)
;; node       node       node       node       the rest of nodes
;; (**)       (x*)       (y*)       (z*)       (************************)
;; (****)     (*x**)     (y***)     (z***)     (************************************************)
;; (********) (**x*****) (*y******) (*z******) (************************************************************************************************)

;; (locat 9 42 69 100)
;; root
;; (**)
;; (****)
;; (********)
;; (**xyz***********)
;; node       node       node1074x  node1074y  node1074z the rest of nodes
;; (**)       (**)       (*x)       (y*)       (z*)      (**********************)
;; (****)     (****)     (**x*)     (*y**)     (*z**)    (********************************************)

;;         nodes                                   node57x         node57y            node57z        the rest of nodes
;; (**) (**) (**) (**) (**) (**) (**) (**) (**) (**) (*x) (**) (**) (y*) (**) (**) (**) (*z) (********************************************************************************************)
