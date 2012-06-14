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

;; SharperMagick - an example program that converts a 2D image to
;; the Sharper directory tree and extracts a region from the Sharper
;; directory tree with different resolution. The program uses
;; Image/Graphics Magick command-line tools.

;; Different versions of ImageMagick work differently for the same
;; command line, e.g. in old version 6.2.8 -extract command works for
;; raw images only. The program in this file does not work at least
;; with this version.

;;; Code:

(defpackage #:sharper-magick
  (:use #:common-lisp #:sharper #:cl-fad)
  (:import-from #:alexandria
                #:with-gensyms))

(in-package #:sharper-magick)

;;; TODO Add checks for the status of a shell command.

(defparameter *tmp-dir* "/tmp/"
  "The directory for temporary files.")

(defvar *tmp-dir-dir* nil
  "The current process unique temporary directory.
The directory is created in the directory `*tmp-dir*' and used for
storing temporary files.")

;;; FIXME: The function `asdf:run-shell-command' is deprecated!
;; (declaim (inline shell-command))
(defun shell-command (control-string &rest args)
  "The function `asdf:run-shell-command' returns implementation
dependent values. E.g. in CLISP `ext:run-shell-command' returns NIL on
success but in SBCL `sb-ext:process-exit-code' returns 0."
  (let ((r (apply #'asdf:run-shell-command control-string args)))
    #+clisp (unless r (setq r 0))
    r))

(defun read-shell-command (control-string &rest args)
  "Read standart output from shell command."
  #+clisp
  (read (ext:run-shell-command (apply #'format nil control-string args)
                               :output :stream :wait t))
  #-clisp
  (read-from-string
   (with-output-to-string (s)
     (let ((asdf::*verbose-out* s))
       (apply #'shell-command control-string args)))))

(defun identify-size (file)
  "Get the image size in the file FILE."
  (values
   (read-shell-command "identify -format '(%w %h)' ~A" file)))

(defun tmpfilename (file res)
  "Make a filename string for the temporary file FILE.
The file have suffix: the resolution RES."
  (namestring
   (dir+file *tmp-dir-dir*
             (format nil "~A~D" (pathname-name file) res))))

(defun extent (file)
  "Make the width and the height of the image equal to power of 2."
  (let* ((r (ceiling (log (apply #'max (identify-size file)) 2)))
         (ext (tmpfilename file r)))
    (if (= (shell-command "convert ~A -background black -extent ~Dx~D png:~A"
                          file (ilength r) (ilength r) ext)
           0)
        (values r ext))))

(defun make-resolutions (file node-res)
  "Make images that are the image FILE at different resolution.
The argument NODE-RES is the resolution of nodes of the created
directory tree."
  (multiple-value-bind (res ext) (extent file)
    (let (files)
      (flet ((mkres (r)
               (shell-command "convert ~A -resize ~D png:~A"
                              ext (ilength r)
                              (car (push (tmpfilename file r) files)))))
        (dotimes (r (1- res))
          (mkres (1+ r)))
        (push ext files)
        (dotimes (r (- (ceil res node-res) res))
          (mkres (+ r res 1)))
        (values (nreverse files) res)))))

(defun extract-pyramid (node loc resfiles)
  "Extract the pyramid from the images RESFILES.
Extract the pyramid for the node NODE. The images RESFILES are made by
the function `make-resolutions'."
  (let* ((res (node-resolution node))
         (pyr (dir+file node *node-pyramid-filename*))
         (offsets (list 0)))
    (if (probe-file pyr) (delete-file pyr))
    (loop
       for r from (+ (locat-r loc) (- res) 1)
       for n from 1 to res
       do (shell-command "convert -extract ~A ~A png:- >> ~A"
                         (apply #'format nil "~Dx~D+~D+~D "
                                (ilength n) (ilength n)
                                (locat-axes (resol r loc)))
                         (namestring (nth (1- r) resfiles))
                         (namestring pyr))
       (push (file-size pyr) offsets))
    (nreverse offsets)))

(defmacro with-tmp-dir (&body body)
  "Execute the forms BODY within the directory *tmp-dir-dir*.
Remove the directory after the last form."
  `(unwind-protect
        (progn (setq *tmp-dir-dir*
                     (dir+file *tmp-dir*
                               (format nil "sharper-magick-~D/"
                                       (random 1000000))))
               (ensure-directories-exist *tmp-dir-dir*)
               ,@body)
     (delete-directory-and-files *tmp-dir-dir*)))

(defun convert2sharper (filename root)
  "Convert the image FILENAME to the Sharper directory tree ROOT."
  (with-tmp-dir
      (multiple-value-bind (resfiles origres)
          (make-resolutions filename sharper:*default-node-resolution*)
        (let* ((res (length resfiles))
               (loc1 (locat res 0 0))
               (loc2 (resol res (apply #'locat origres
                                       (identify-size filename)))))
          (create-nodes-box root loc1 loc2
                            #'(lambda (node loc)
                                (write-file
                                 (list
                                  (cons :offsets
                                        (butlast
                                         (extract-pyramid node loc resfiles))))
                                 (dir+file node
                                           sharper:*node-properties-filename*)
                                 :supersede)))))))

;;; TODO Maybe use montage?
(defun convert2file (root filename &optional res loc1 loc2)
  "Convert the Sharper directory tree ROOT to the image FILENAME.
If the resolution RES is NIL that the resolution of the converted
image will be the maximum resolution of the directory tree.

The arguments LOC1 and LOC2 specify the region extracted from the
directory tree. LOC1 specifies the nearest to the image origin
corner. LOC2 specifies the farthest corner. If both LOC1 and LOC2 are
NIL the region has size of the image FILENAME."
  (let* ((res (unless* res (dtree-resolution root)))
         (size (ilength res))
         (loc1 (unless* loc1 (locat res 0 0)))
         (loc2 (unless* loc2 (locat res (1- size) (1- size)))))
    (shell-command "convert -size ~Dx~D canvas:black ~A"
                   size size filename)
    (labels
        ((pixel (r x y)
           "Extract and resize pixel at lower resolution if the
node at the resolution RES is not available."
           (format nil
                   " | convert +repage - -~
                     | convert -extract 1x1+~D+~D - -resize ~Dx~D -"
                   x y (ilength r) (ilength r)))
         (composite (node tile loc)
           "Put image from the pyramid of the node NODE at the
location LOC in the image FILENAME."
           (shell-command "tail -c +~D ~A ~A | composite ~A - ~A ~A"
                          (1+
                           (nth (1- (locat-r tile))
                                (node-prop node :offsets)))
                          (dir+file node
                                    sharper:*node-pyramid-filename*)
                          (if (< (locat-r loc) res)
                              (apply #'pixel
                                     (- res (locat-r loc))
                                     (locat-axes tile))
                              "")
                          (apply #'format nil "-geometry +~D+~D"
                              (locat-axes (resol res loc)))
                          filename filename)))
      (find-nodes-box root loc1 loc2 #'composite))))

(defun convert (image pathname &optional resolution loc1 loc2)
  "Convert the image IMAGE to or from the Sharper directory tree.

The other arguments:

 - PATHNAME
   If IMAGE is a pathname which designates a file, convert it to the
   Sharper directory tree PATHNAME. If IMAGE is a pathname which
   designates a directory that is the Sharper directory tree, convert
   it to the file PATHNAME, in which case the file image format of it
   determined by its suffix (extension).

 - RESOLUTION
   Resolution of the converted image.

 - LOC1, LOC2
   The arguments specify the region extracted from the directory
   tree. LOC1 specifies the nearest to the image origin corner. LOC2
   specifies the farthest corner. If both LOC1 and LOC2 are NIL the
   region has size of the image IMAGE."
  (if (directory-pathname-p image)
      (convert2file image pathname resolution loc1 loc2)
      (convert2sharper image pathname)))

;;; TODO Parse command-line arguments and run conversion.
