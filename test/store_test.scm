(use-modules (docfiler store)
             (docfiler tap)
             (docfiler test)
             (oop goops describe)
             (oop goops))

(define test (make <tap-test-collection>))

(register-store-tests test (make-store 'memory))

(let* ((base-path (test-make-path))
       (recipients '("test-user1" "test-user2"))
       (gpg-home (apply test-setup-gpg-keychain recipients)))
  (register-store-tests test (make-store 'fs base-path recipients #:gpg-home gpg-home))
  (test-cleanup-paths base-path gpg-home))

(run-tests test)

(define (register-store-tests test store)
  (store-upsert store
                '("2019-02-01" "my-doc")
                '(("name" . "My doc")
                  ("tags" . "apartment,bills")))

  (let ((props (store-get store '("2019-02-01" "my-doc"))))
    (register-test
     test "can fetched saved properties"
     (is-eq "My doc" (assoc-ref props "name")))

    (register-test
     test "can fetched saved properties"
     (is-eq "apartment,bills" (assoc-ref props "tags"))))

  (register-test
   test "non-existant key returns nil"
   (is-ok (null? (store-get store '("baz" "foo")))))

  ;; Test the merging of properties.
  (store-upsert store
                '("2019-02-01" "my-doc")
                '(("tags" . "water bill,apartment,bills")))
  (let ((updated-props (store-get store '("2019-02-01" "my-doc")))
        (found #f)
        (found-filtered #f))
    (register-test
     test "can fetched unmodified properties"
     (is-eq "My doc" (assoc-ref updated-props "name")))

    (register-test
     test "property modified properly"
     (is-eq "water bill,apartment,bills" (assoc-ref updated-props "tags")))

    (store-iterate store (lambda (k) (set! found #t)))
    (register-test
     test "iteration works"
     (is-ok found))

    (store-iterate store (lambda (k) (set! found-filtered #t)) "2019-02-01")
    (register-test
     test "filtered iteration works"
     (is-ok found-filtered))))
