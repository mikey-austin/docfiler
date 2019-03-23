;;; Commentary:

;; This module implements an in-memory adapter for the store
;; interface; it is primarily useful for testing.

;;; Code:

(define-module (docfiler store memory-adapter)
  #:use-module (oop goops)
  #:use-module (scheme documentation)
  #:export (<doc-store-memory-adapter>
            adapter-store-upsert))

(define-class-with-docs <doc-store-memory-adapter> ()
  "\
This class represents the memory adapter, and manages the
in-memory file store.
"
  (file-props #:init-thunk make-hash-table #:getter file-props))

(define-generic-with-docs adapter-store-upsert
  "\
Implement memory-backed upsert functionality.
")

(define-method (adapter-store-upsert (self <doc-store-memory-adapter>)
                             (doc-key <list>)
                             (props <list>))
  (let ((existing-props (hash-ref (file-props self) doc-key (make-hash-table))))
    (for-each
     (lambda (new-prop)
       (hash-set! existing-props (car new-prop) (cdr new-prop))) props)
    (hash-set! (file-props self) doc-key existing-props)))

;;; memory-adapter.scm ends here.
