;;; utility.lisp --- Utility functions

;;; Copyright (C) 2018 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; This is a collection of functions that are not assignments, but are used
;;; in the code.
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

(in-package :net.spauldo.ai-text.utility)

;; Count atoms in a list
;; This function is not tail recursive!
(defun count-atoms (item)
  "Returns the number of atoms in ITEM.  ITEM may be an atom or a list.
Do note that a NIL at the end of a CONS cell is never counted, even if
if it is written as '(X . NIL)."
  ;; We can't use reduce, because it can't handle improper lists.
  (cond ((atom ITEM) 1)
	((listp ITEM)
	 (+ (count-atoms (car ITEM))
	    (if (eq (cdr ITEM) nil)
		0
		(count-atoms (cdr ITEM)))))
	(t
	 (error "ITEM is not a list or atom: ~s" ITEM))))
