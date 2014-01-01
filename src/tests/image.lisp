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

;;; Code:

(in-package #:sharper-test)

;; (walk-image-box tile img (get-loc1 img) (get-loc2 img)
;;   (prepare-smooth tile)
;;   (smooth-tile tile))

(defun write-tiles (node &rest tiles)
  "Write TILES to NODE.
Each element of TILES is a cons (RES . VECTOR)"
  (mapc #'(lambda (tl)
            (setf (sharper::node-tile node (car tl)) (cdr tl)))
        tiles))

;;; The test dtree is not created
(deftest walk-image-box-read-test1
    (let (tiles)
      (walk-image-box tile "/tmp/root/" (locat 3 1 4 5) (locat 3 7 2 4)
        (push (list (vec2str tile)
                    tile-res tile-loc
                    tile-bl1 tile-bl2)
              tiles)
        (push (list tile-res tile-loc
                    tile-bl1 tile-bl2
                    tile-low-res
                    tile-low-loc
                    tile-loc-low)
              tiles)
        tiles))
  (3 (3 0 0 0) (3 1 2 4) (3 7 4 5) nil nil nil))

(defun str2vec (s)
  "Convert the string S to a byte vector."
  (map '(vector (unsigned-byte 8)) #'(lambda (c) (char-code c)) s))

(defun vec2str (v)
  "Convert the byte vector V to a string."
  (if v (map 'string #'(lambda (c) (code-char c)) v)))

;;; There is root node only
(deftest walk-image-box-read-test2
  (with-root
    (ensure-directories-exist "/tmp/root/")
    (write-node "/tmp/root/" 4 3)
    (dolist (r (list 1 3 4))
      (setf (sharper::node-tile "/tmp/root/" r)
            (str2vec (format nil "This is tile number %D" r))))
    (values
     (let ((l1 (locat 5 1 24 11))
           (l2 (locat 5 6 17 31))
           (tiles))
       (for (r 1 (<= r 5) (incf r))
         (walk-image-box
             tile "/tmp/root/"
             (resol r l1) (resol r l2)
           (push (list (vec2str tile)
                       tile-res tile-loc
                       tile-bl1 tile-bl2)
                 tiles)
           (push (list tile-res tile-loc
                       tile-bl1 tile-bl2
                       tile-low-res
                       tile-low-loc
                       tile-loc-low)
                 tiles)))
       tiles)))
  ("This is tile number 1" 1 (1 0 0 0) (1 0 1 0) (1 0 1 1))
  ;; `walk-image-box' can't distinguish tiles data for the created
  ;; subtree, it evaluates FORM1, but from user's point of view FORM2
  ;; should be evaluated.  This behavour should be implemented anyway
  ;; because dtree resolution maybe greater or equal to maximum image
  ;; resolution, i.e. dtree resolution grows with nodes resolution and
  ;; user may have data on resolution R - N, where R is dtree
  ;; resolution and N < leaf nodes resolution.  If user reads tiles on
  ;; R he/she expects to FORM2 should be evaluated but
  ;; `find-nodes-box' funcalls its first closure because it find nodes
  ;; for target resolution.  In such a case `walk-image-box' should
  ;; store in nodes info about empty tiles.  Also it should find the
  ;; lower resolution tile.
  (2 (2 0 0 0) (2 0 2 1) (2 0 3 3) 1 (1 0 0 0) (1 0 0 0))
  ("This is tile number 3" 3 (3 0 0 0) (3 0 4 2) (3 1 6 7))
  ("This is tile number 4" 4 (4 0 0 0) (4 0 8 5) (4 3 12 15))
  (5 (5 0 0 0) (5 1 17 11) (5 6 24 31) 4 (4 0 0 0) (4 0 8 5)))

;;; There is no lower resolution
(deftest walk-image-box-read-test3
  (with-root
    (ensure-directories-exist "/tmp/root/")
    (write-node "/tmp/root/" 4 3)
    (setf (sharper::node-tile "/tmp/root/" 3)
          (str2vec "This is tile number 3"))
    (let (tiles)
      (walk-image-box
          tile "/tmp/root/"
          (locat 2 1 2 3) (locat 2 4 5 6)
        (push (list (vec2str tile)
                    tile-res tile-loc
                    tile-bl1 tile-bl2)
              tiles)
        (push (list tile-res tile-loc
                    tile-bl1 tile-bl2
                    tile-low-res
                    tile-low-loc
                    tile-loc-low)
              tiles))
      tiles))
  (2 (2 0 0 0) (2 1 2 3) (2 4 5 6) nil nil nil))

;;; Read tiles in kids
(deftest walk-image-box-read-test4
    (with-root
      (ensure-directories-exist "/tmp/root/")
      (write-node "/tmp/root/" 4 3)
      (let (tiles-read
            tiles-root
            tiles-low
            (l1 (locat 7 3 9 17))
            (l2 (locat 7 76 43 100))
            (kidsnotiles (mapcar #'locat
                                 (make-list 7 :initial-element 4)
                                 '((0 1 2) (1 1 2) (5 2 5) (6 2 5)
                                   (5 3 6) (6 3 7) (7 5 12))))
            (kidslowtiles (mapcar #'locat
                                 (make-list 6 :initial-element 4)
                                 '((9 1 3) (9 2 3) (1 3 9)
                                   (2 3 9) (1 4 8) (9 5 12)))))
        (create-kids l1 l2 3 "/tmp/root/" 4)
        ;; Kids without tiles and no low res
        ;;
        ;; Kids without tiles and low res in root
        ;; ((0 1 ) (1 1 ) (5 2 ) (6 2 ) (5 3 ) (6 3 ) (7 5 ))
        ;; Kids with tiles not on the target resolution
        ;; ((9 1 ) (9 2 ) (1 3 ) (2 3 ) (1 4 ) (9 5 ))
        ;; Kids without tiles and no low res
        ;; Out of the box
        (setf (node-tile "/tmp/root/" 3)
              (str2vec "This is root tile at resolution 3!"))
        (walk-box (resol 4 l1) (resol 4 l2)
                  #'(lambda (l)
                      (cond
                        ((member l kidsnotiles))
                        ((member l kidslowtiles)
                         (setf (node-tile (find-node "/tmp/root/" l) 1)
                               (str2vec
                                (format nil
                                        "This is a low res tile at loc ~A"
                                        l))))
                        (t (setf (node-tile (find-node "/tmp/root/" l) 1)
                                 (str2vec
                                  (format nil
                                          "This is a tile at loc ~A" l)))))))
        (walk-image-box))))

(defmacro with-root-res (res &body body)
  "Execute BODY with temporary image and node resolution RES."
  (let (oldres (gensym))
    `(let ((,oldres sharper::*new-node-resolution*))
       (unwind-protect
            (with-root
              (setq sharper::*new-node-resolution* ,res)
              ,@body)
         (setq sharper::*new-node-resolution* ,oldres)))))
