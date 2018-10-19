;;; quasi-balancedp.lisp --- Determine if a list is "quasi-balanced"
;;;
;;; Copyright (C) 2018 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; Lessons learned:
;;;
;;; LIST-LENGTH is defined in :COMMON-LISP, apparently.  Good to know.
;;;
;;; This one was actually a bit tricky at first.  As usual, I was overthinking
;;; it.  Once I decided to just attack each requirement separately, the solution
;;; basically write itself.
;;;
;;; This is rather inefficient, however, as it needs to traverse each list more
;;; than once, and does not exit immediately upon finding an unbalanced list.
;;;
;;; Perhaps combining LIST-LEN and LIST-DEPTH and returning multiple values
;;; would be a good way to go.
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

(in-package :net.spauldo.ai-text.quasi-balancedp)

(defun list-len (lst)
  "Get the length of the list LST.  Atoms (and '()) are length 0."
  (if (atom lst)
      0
      (+ 1 (list-len (cdr lst)))))

(defun max-int (lst)
  "Return the maximum positive integer from list LST."
  (cond ((not lst)
	 0)
	((integerp lst)
	 lst)
	((listp lst)
	 (if (integerp (car lst))
	     (let ((cdrval (max-int (cdr lst))))
	       (if (> (car lst) cdrval)
		   (car lst)
		   cdrval))
	     (error "Not an integer: ~a" (car lst))))
	(T
	 (error "Not NIL, an integer, or a list: ~a" lst))))

(defun list-depth (lst)
  "Return the nesting depth of LST."
  (if (atom lst)
      0
      (1+ (max-int (mapcar #'list-depth lst)))))

(defun quasi-balancedp (lst)
  "Return T if LST is quasi-balanced; otherwise, return NIL."
  (and (if (atom lst) ; Test for equal length
	   T
	   (equalelts (mapcar #'list-len lst)))
       (if (atom lst) ; Test for equal depth
	   T
	   (equalelts (mapcar #'list-depth lst)))
       (if (atom lst) ; Test that all sublists are quasi-balanced
	   T
	   (equalelts (mapcar #'quasi-balancedp (cons T lst))))))

(defun test ()
  (and (quasi-balancedp 'A)
       (quasi-balancedp '((A B) (C D) (E F)))
       (quasi-balancedp '(((A B) (C D)) ((E F) (G H))))
       (not (quasi-balancedp '((A (B C)) (D E) (F G))))))
