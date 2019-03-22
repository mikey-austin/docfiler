;;; Commentary:

;; This module comprises the document storage interface for working
;; with docfiler files. It is designed to be agnostic of the underlying
;; workings via a "storage adapter" abstraction.

;;; Code:

(define-module (docfiler store)
  #:use-module (oop goops)
  #:use-module (docfiler store fs-adapter)
  #:duplicates (merge-generics)
  #:export (<doc-store>
            store-make-fs-store))

(define-class <doc-store> ()
  (adapter #:init-keyword #:adapter))

(define (store-make-fs-store . fs-adapter-params)
  "\
Create an fs-backed store. For parameter documentation, see
the `(docfiler store fs-adapter):storage-make-fs-adapter'.
"
  (let ((fs-adapter (apply storage-make-fs-adapter fs-adapter-params)))
    (make <doc-store> #:adapter fs-adapter)))

;;; store.scm ends here.
