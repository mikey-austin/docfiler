;;; Commentary:

;; This module implements the storage interface generic
;; functions for the fs backend. All storage operations
;; are encrypted via the gpg module.

;;; Code:

(define-module (docfiler store fs-adapter)
  #:use-module (oop goops)
  #:use-module (docfiler fs)
  #:use-module (docfiler utils)
  #:use-module (docfiler gpg)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)
  #:use-module (scheme documentation)
  #:export (<doc-store-fs-adapter>
            store-make-fs-adapter
            adapter-store-get
            adapter-store-iterate
            adapter-store-upsert))

(define-class <doc-store-fs-adapter> ()
  (fs #:init-keyword #:fs #:getter get-fs))

(define* (store-make-fs-adapter base-path recipients #:key
                                gpg-prog gpg-home)
  "\
Make a new storage fs adapter instance, configure to encrypt
all creations/modifications for the supplied gpg recipients. All
store operations will happen from the supplied base path. The
gpg program path and gpg home directory arguments are optional.
"
  (let* ((fs (make <doc-fs>
               #:base-path base-path
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

(define (doc-key->rel-path doc-key)
  (apply join-paths (append (string-split (car doc-key) #\-)
                            (cdr doc-key))))

(define (rel-path->doc-key rel-path)
  (let* ((components (string-split rel-path #\/))
         (date (string-join (take components 3) "-"))
         (filename (drop components 3)))
    (list date filename)))

(define-method (adapter-store-upsert (self <doc-store-fs-adapter>)
                                     (doc-key <list>)
                                     (props <list>))
  (let* ((doc-path (doc-key->rel-path doc-key))
         (fs (get-fs self))
         (existing-props (alist->hash-table (fs-load-props fs doc-path))))
    (for-each
     (lambda (prop)
       (hash-set! existing-props (car prop) (cdr prop)))
     props)
    (fs-save-props fs doc-path (hash-map->list cons existing-props))))

(define-generic-with-docs adapter-store-get
  "\
Return an alist of properties associated with the specified key. If there
are no such properties (eg the file doesn't exist), then the empty list is
returned.
")

(define-method (adapter-store-get (self <doc-store-fs-adapter>)
                                  (doc-key <list>))
  (let ((doc-path (doc-key->rel-path doc-key))
        (fs (get-fs self)))
    (fs-load-props fs doc-path)))

(define-generic-with-docs adapter-store-iterate
  "\
Iterate over all document keys applying the supplied procedure
over each key if no key prefixes were given, or the current key
prefix is in the speficied list.
")

(define-method (adapter-store-iterate (self <doc-store-fs-adapter>)
                                      (proc <procedure>)
                                      . key-prefixes)
  (fs-for-each
   (get-fs self)
   (lambda (rel-path)
     (let ((doc-key (rel-path->doc-key rel-path)))
       (if (or (nil? key-prefixes)
               (member (car doc-key) key-prefixes))
           (proc doc-key))))))

;;; fs-adapter.scm ends here.
