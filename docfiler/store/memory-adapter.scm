;;; Commentary:

;; This module implements an in-memory adapter for the store
;; interface; it is primarily useful for testing.

;;; Code:

(define-module (docfiler store memory-adapter)
  #:use-module (oop goops)
  #:use-module (scheme documentation)
  #:use-module (ice-9 hash-table)
  #:export (<doc-store-memory-adapter>
            adapter-store-get
            adapter-store-iterate
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

(define-generic-with-docs adapter-store-get
  "\
Return an alist of properties associated with the specified key. If there
are no such properties (eg the file doesn't exist), then the empty list is
returned.
")

(define-method (adapter-store-get (self <doc-store-memory-adapter>)
                                  (doc-key <list>))
  (let ((existing-props (hash-ref (file-props self) doc-key)))
    (if existing-props
        (hash-map->list (lambda (k v) `(,k . ,v)) existing-props)
        '())))

(define-generic-with-docs adapter-store-iterate
  "\
Iterate over all document keys applying the supplied procedure
over each key if no key prefixes were given, or the current key
prefix is in the speficied list.
")

(define-method (adapter-store-iterate (self <doc-store-memory-adapter>)
                                      (proc <procedure>)
                                      . key-prefixes)
  (let ((prefixes (car key-prefixes)))
    (hash-for-each
     (lambda (k v)
       (if (or (nil? prefixes)
               (member (car k) prefixes))
           (proc k)))
     (file-props self))))

;;; memory-adapter.scm ends here.
