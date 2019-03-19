;; Commentary:

;; This module contains the classes and functions to maintain a docfiler
;; filesystem database structure.

;; Code:

(define-module (docfiler fs)
  #:use-module (oop goops)
  #:export (<doc-fs> get-fs-base-path))

(define-class <doc-fs> ()
  (base-path #:getter get-fs-base-path))


