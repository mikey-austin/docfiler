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

(define props (store-get store '("2019-02-01" "my-doc")))
(register-test
 test "can fetched saved properties"
 (is-eq "My doc" (assoc-ref props "name")))

(register-test
 test "can fetched saved properties"
 (is-eq "apartment,bills" (assoc-ref props "tags")))

(register-test
 test "non-existant key returns nil"
 (is-ok (null? (store-get store '("baz" "foo")))))

;; Test the merging of properties.
(store-upsert store
              '("2019-02-01" "my-doc")
              '(("tags" . "water bill,apartment,bills")))
(define updated-props (store-get store '("2019-02-01" "my-doc")))

(register-test
 test "can fetched unmodified properties"
 (is-eq "My doc" (assoc-ref updated-props "name")))

(register-test
 test "property modified properly"
 (is-eq "water bill,apartment,bills" (assoc-ref updated-props "tags")))

(run-tests test)
