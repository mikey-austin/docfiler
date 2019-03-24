;;; Commentary:

;; This module comprises the document storage interface for working
;; with docfiler files. It is designed to be agnostic of the underlying
;; workings via a "storage adapter" abstraction.

;;; Code:

(define-module (docfiler store)
  #:use-module (oop goops)
  #:use-module (docfiler store fs-adapter)
  #:use-module (docfiler store memory-adapter)
  #:use-module (scheme documentation)
  #:duplicates (merge-generics)
  #:export (<doc-store>
            make-store
            store-get
            store-iterate
            store-upsert))

(define-class-with-docs <doc-store> ()
  "\
This class holds the necessary state to dispatch calls to the relevant
storage adapters.
"
  (adapter #:init-keyword #:adapter #:getter get-adapter))

(define (make-store adapter-type . adapter-params)
  "\
Make a store instance of the specified type. The adapter
params are specific to the chosen type. The following are
the supported adapter type symbols:

@itemize

@bullet @code{fs} - make a file-system backed store

@bullet @code{memory} - make a memory-backed store

@end itemize
"
  (let ((adapter (case adapter-type
                   ((fs) (apply store-make-fs-adapter adapter-params))
                   ((memory) (make <doc-store-memory-adapter>))
                   (else (throw 'unknown-adapter-type)))))
    (make <doc-store> #:adapter adapter)))

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
  (adapter-store-upsert (get-adapter self) doc-key props))

(define-generic-with-docs store-get
  "\
Retrieve all properties associated with the supplied key. See the
@code{store-upsert} docs for details on the format of the @code{doc-key}.

The props are returned as an association list.
")

(define-method (store-get (self <doc-store>)
                          (doc-key <list>))
  (adapter-store-get (get-adapter self) doc-key))

(define-generic-with-docs store-iterate
  "\
Iterate over the specified key prefixes executing the supplied
proc with each document key as an argument.
")

(define-method (store-iterate (self <doc-store>)
                              (proc <procedure>)
                              . key-prefixes)
  (adapter-store-iterate (get-adapter self) proc key-prefixes))

;;; store.scm ends here.
