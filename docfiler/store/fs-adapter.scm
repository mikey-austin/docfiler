;;; Commentary:

;; This module implements the storage interface generic
;; functions for the fs backend. All storage operations
;; are encrypted via the gpg module.

;;; Code:

(define-module (docfiler store fs-adapter)
  #:use-module (oop goops)
  #:use-module (docfiler fs)
  #:use-module (docfiler gpg)
  #:use-module (ice-9 popen)
  #:export (<doc-store-fs-adapter>
            storage-make-fs-adapter))

(define-class <doc-store-fs-adapter> ()
  (fs #:init-keyword #:fs))

(define* (storage-make-fs-adapter base-path recipients #:key
                                  gpg-prog gpg-home)
  "\
Make a new storage fs adapter instance, configure to encrypt
all creations/modifications for the supplied gpg recipients. All
store operations will happen from the supplied base path. The
gpg program path and gpg home directory arguments are optional.
"
  (let* ((fs (make <doc-fs>
               #:port-closer close-pipe
               #:meta-filename "meta.gpg"
               #:in-port (lambda (abs-path)
                           (gpg-decrypt abs-path
                                        #:gpg-prog gpg-prog
                                        #:gpg-home gpg-home))
               #:out-port (lambda (abs-path)
                            (gpg-encrypt recipients abs-path
                                         #:gpg-prog gpg-prog
                                         #:gpg-home gpg-home)))))
    (make <doc-storage> #:fs fs)))

;;; fs-adapter.scm ends here.
