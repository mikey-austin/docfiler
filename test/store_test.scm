(use-modules (docfiler store)
             (docfiler tap)
             (oop goops describe)
             (oop goops))

(define test (make <tap-test-collection>))

(define store (make-store 'memory))

(register-test
 test "the memory-backed store is created correctly"
 (is-ok (not (null? store))))

(store-upsert store
              '("2019-02-01" "my-doc")
              '(("name" . "My doc")
                ("tags" . "apartment,bills")))

(run-tests test)
