(use-modules (docfiler fs)
             (docfiler tap)
             (oop goops))

(define t (make <tap-test-collection>))

(let ((fs-base-path "/tmp/test_doc_filer_base")
      (fs (make <doc-fs>)))
  (slot-set! fs 'base-path fs-base-path)
  (register-test t "the base-path setter works"
                 (is-eq (slot-ref fs 'base-path) fs-base-path)))

(run-tests t)
