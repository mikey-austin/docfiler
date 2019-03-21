;;; Commentary:

;; This module defines the functions for adding GPG encryption
;; support to the fs primitives.

;;; Code:

(define-module (docfiler gpg)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1)
  #:export (gpg-encrypt
            gpg-decrypt
            gpg-encrypt-cmd
            gpg-decrypt-cmd))

;; TODO: use autotools substitution here.
(define default-gpg-prog "/usr/bin/gpg2")

(define (gpg-expand-recipients recipients)
  (fold-right
   (lambda (a b) (cons "-r" (cons a b))) '() recipients))

(define* (gpg-encrypt-cmd recipients abs-path #:key
                          (gpg-prog default-gpg-prog)
                          gpg-home)
  (append (list gpg-prog "-ea")
          (gpg-expand-recipients recipients)
          (if gpg-home ("--homedir" gpg-home) '())
          (list "--batch" "--yes" "--output" abs-path)))

(define* (gpg-decrypt-cmd abs-path #:key
                          (gpg-prog default-gpg-prog)
                          gpg-home)
  (append (list gpg-prog "--batch" "--yes" "-d")
          (if gpg-home ("--homedir" gpg-home) '())
          (list abs-path)))

(define* (gpg-encrypt recipients abs-path #:key
                      (gpg-prog default-gpg-prog)
                      gpg-home)
  "\
Return a writable port that encrypts data written to it to
abs-path for the specified list of recipients. Note this stream
must be closed with `close-pipe' or else it will leave a bunch
of zombies everywhere."
  (apply open-pipe* (cons OPEN_WRITE
                          (gpg-encrypt-cmd recipients abs-path
                                           #:gpg-prog gpg-prog
                                           #:gpg-home gpg-home))))

(define* (gpg-decrypt abs-path #:key
                      (gpg-prog default-gpg-prog)
                      gpg-home)
  "\
Return a readable port that supplies decrypted data from
abs-path to the caller. Note this stream
must be closed with `close-pipe' or else it will leave a bunch
of zombies everywhere."
  (apply open-pipe* (cons OPEN_READ
                          (gpg-decrypt-cmd abs-path
                                           #:gpg-prog gpg-prog
                                           #:gpg-home gpg-home))))

;;; gpg.scm ends here.
