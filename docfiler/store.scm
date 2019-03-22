;;; Commentary:

;; This module comprises the document storage interface for working
;; with docfiler files. It is designed to be agnostic of the underlying
;; workings via a "storage adapter" abstraction.

;;; Code:

(define-module (docfiler store)
  #:use-module (oop goops)
  #:use-module (docfiler store fs-adapter)
  #:use-module (scheme documentation)
  #:duplicates (merge-generics)
  #:export (<doc-store>
            store-make-fs-store
            store-upsert))

(define-class-with-docs <doc-store> ()
  "\
This class holds the necessary state to dispatch calls to the relevant
storage adapters.
"
  (adapter #:init-keyword #:adapter #:getter get-adapter))

(define (store-make-fs-store . fs-adapter-params)
  "\
Create an fs-backed store. For parameter documentation, see
the `(docfiler store fs-adapter):storage-make-fs-adapter'.
"
  (let ((fs-adapter (apply storage-make-fs-adapter fs-adapter-params)))
    (make <doc-store> #:adapter fs-adapter)))

(define-generic-with-docs store-upsert
  "\
Update or insert a document and it's properties. If the document exists
the specified props will be merged with the existing properties and will
be saved, else it will be created.

The supplied `doc-key' is a list of the form:

   (\"YYYY-mm-dd\" \"my_doc_name\")

The supplied `props' parameter is an association list of string keys and
string values.
")

(define-method (store-upsert (self <doc-store>)
                             (doc-key <list>)
                             (props <list>))
  (store-upsert (get-adapter self) doc-key props))

;;; store.scm ends here.
