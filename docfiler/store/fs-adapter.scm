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
  #:use-module (scheme documentation)
  #:export (<doc-store-fs-adapter>
            store-make-fs-adapter
            adapter-store-get
            adapter-store-upsert))

(define-class <doc-store-fs-adapter> ()
  (fs #:init-keyword #:fs))

(define* (store-make-fs-adapter base-path recipients #:key
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
    (make <doc-store-fs-adapter> #:fs fs)))

(define-method (adapter-store-upsert (self <doc-store-fs-adapter>)
                                     (doc-key <list>)
                                     (props <list>))
  ;; TODO: implement this interface method:
  ;;        - convert doc key to fs path
  ;;        - load properties from path
  ;;        - merge supplie props into loaded props
  ;;        - save the properties
  (throw 'not-implemented))

(define-generic-with-docs adapter-store-get
  "\
Return an alist of properties associated with the specified key. If there
are no such properties (eg the file doesn't exist), then the empty list is
returned.
")

(define-method (adapter-store-get (self <doc-store-fs-adapter>)
                                  (doc-key <list>))
  (throw 'not-implemented))

;;; fs-adapter.scm ends here.
