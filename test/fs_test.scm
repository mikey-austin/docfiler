(use-modules (docfiler fs)
             (docfiler tap)
             (oop goops)
             (ice-9 ftw))

(define test (make <tap-test-collection>))

(register-test
 test "ensure path joining works"
 (is-eq "a/b/c/d" (join-paths "a////" "b" "//c////" "d")))

(define fs (make <doc-fs>))
(define tmp-base
  (join-paths "/tmp"
              (string-append "test_" (number->string (random 1000000000)))))
(slot-set! fs 'base-path tmp-base)

(register-test
 test "abs path creation works"
 (is-eq (join-paths tmp-base "baz") (fs-make-abs-path fs "baz")))

(define test-meta-path (join-paths (fs-make-abs-path fs "foobar") "test_meta"))
(register-test
 test "we can write out an object"
 (is-ok (fs-write-abs-path fs test-meta-path '(1 2 3 4))))

(register-test
 test "we can read back our written object"
 (is-eq '(1 2 3 4) (fs-read-abs-path fs test-meta-path)))

(register-test
 test "we can set properties"
 (is-ok (fs-set-prop fs "foobar" "first-name" "John")))

(register-test
 test "we can set properties"
 (is-ok (fs-set-prop fs "foobar" "last-name" "Smith")))

(register-test
 test "we can get a property back"
 (is-eq "John" (fs-get-prop fs "foobar" "first-name")))

(register-test
 test "we can get a property back"
 (is-eq "Smith" (fs-get-prop fs "foobar" "last-name")))

(register-test
 test "we can override a property"
 (is-ok (fs-set-prop fs "foobar" "first-name" "Mike")))

(register-test
 test "we can get the overridden property back"
 (is-eq "Mike" (fs-get-prop fs "foobar" "first-name")))

(run-tests test)

;; Cleanup the temporary files and directories.
(nftw tmp-base
      (lambda (filename statinfo flag base level)
        (case flag
          ((directory-processed) (rmdir filename))
          ((regular) (delete-file filename)))
        #t)
      'depth)
