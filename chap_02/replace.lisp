;;; replace.lisp
;;;
;;; Lesson learned:
;;; Common Lisp apparently doesn't allow you to define a symbol with the
;;; same name as a symbol in package COMMON-LISP (even if you're defining
;;; it in a different package).
;;;
;;; Contrast that with Scheme, where you can redefine everything to your
;;; heart's content.
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

(in-package :net.spauldo.ai-text.replace)

(defun replace-elt (s1 s2 s3)
  "Replace all occurrences of S2 with S3 in S1."
  (mapcar (lambda (elt)
	    (cond ((equal elt s2)
		   s3)
		  ((listp elt)
		   (replace-elt elt s2 s3))
		  (t elt)))
	  s1))

(defun test ()
  (and (equal (replace-elt '(a b c d e f g)
			   'a
			   'alpha)
	      '(alpha b c d e f g))
       (equal (replace-elt '(1 2 3 4 5 (1 2 3 4 5))
			   '2
			   '6)
	      '(1 6 3 4 5 (1 6 3 4 5)))
       (equal (replace-elt '((this 1) contains (2 occurrences (this 1)))
			   '(this 1)
			   '(that one))
	      '((that one) contains (2 occurrences (that one))))
       (equal (replace-elt '((1 2 (this 1) 3) (this 1) (((3 (this 1)) 4)))
			   '(this 1)
			   '(that one))
	      '((1 2 (that one) 3) (that one) (((3 (that one)) 4))))))
