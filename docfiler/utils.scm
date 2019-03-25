;;; Commentary:

;; This module defines various utility routines.

;;; Code:

(define-module (docfiler utils)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (join-paths
            mkdir-p))

(define (join-paths . paths)
  "Join a list of string path components into a clean unixy
filesystem path, with duplicate slashes removed"
  (regexp-substitute/global #f "/+" (string-join paths "/") 'pre "/" 'post))

(define (mkdir-p path)
  "Create a new directory including all ancestors from path"
  (fold
   (lambda (component path-so-far)
     (let ((next-ancestor (join-paths path-so-far component)))
       (unless (file-exists? next-ancestor) (mkdir next-ancestor))
       next-ancestor))
   "" (string-split path #\/)))

;;; utils.scm ends here.
