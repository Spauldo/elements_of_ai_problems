;;; exify.lisp
;;;
;;; Lesson learned:
;;; MAPCAR (and many of the sequence functions) doesn't handle improper
;;; lists.
;;;
;;; That makes sense, really, but I had never tried it before.
;;;
;;; Copyright (C) 2018 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; This is the ISC License.
;;;
;;; Permission to use, copy, modify, and distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defpackage :net.spauldo.ai-text.exify
  (:use :common-lisp)
  (:export :test))

;;; First attempt, doesn't handle improper lists or single atoms

;; (defun exify (l)
;;   "Recursively replace all non-nil atoms in a list with the symbol X."
;;   (mapcar (lambda (elt)
;; 	    (cond ((listp elt) (exify elt))
;; 		  ((atom elt) 'X)
;; 		  ((not elt) nil)
;; 		  (t (error "elt is not nil, a list, or an atom"))))
;; 	  l))

;;; Second attempt - works but uses non-tail-recursive calls which will be a
;;; problem with long lists.
(defun exify (item)
  "Recursively replace all non-nil atoms in ITEM with the symbol X."
  (cond ((not item) nil)
	((atom item) 'X)
	((listp item) (cons (exify (car item)) (exify (cdr item))))
	(t (error "item is not nil, a list, or an atom."))))

;; Test condition
(defun test ()
  "Tests EXIFY using the condition given in the textbook."
  (and (equal (exify '(A (B . C) X Y NIL Z))
	      '(X (X . X) X X NIL X))
       ;; Additional test for atoms
       (equal (exify 'blah)
	      'X)))

