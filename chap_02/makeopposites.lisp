;;; makeopposites.lisp --- Question 18 of chapter 2 of Elements of AI

;;; Copyright (C) 2018 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; The example given in the book is pretty much just a mapcar with a cond,
;;; and each word is compared explicitly in the cond.  There's a lot of
;;; repetition.
;;;
;;; So we're not going to do that.  This is LISP, not C.  We have better
;;; options.
;;;
;;; I'm using alists.  But hey, we need reverse mapping as well.  So I wrote
;;; a function that copied all the items in an alist, swapped the keys and
;;; values, and appended it to the list.
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

(in-package :net.spauldo.ai-text.makeopposites)

(defparameter *opposite-alist*
  '((IS . ISNT)
    (ARE . ARENT)
    (UP . DOWN)
    (LEFT . RIGHT) ; could lead to interesting sentences
    (FORWARD . BACKWARD)
    (BLACK . WHITE)
    (GOOD . BAD)
    (SAD . HAPPY)
    (ANGRY . CALM)
    (DRUNK . SOBER)
    (HOT . COLD)
    (CLEAN . DIRTY)
    (FIRST . LAST))
  "An alist of words and their opposites.")

(defun add-reverse-mapping (alist)
  "Takes ALIST, reverses the keys and values, and appends it to ALIST."
  (let ((alist-rev (mapcar (lambda (item) (cons (cdr item) (car item)))
			   alist)))
    (append alist alist-rev)))

(defun makeopposites (lst)
  "Replaces symbols in LST with their opposites."
  (mapcar (lambda (word)
	    (let ((opposite
		   (cdr (assoc word (add-reverse-mapping *opposite-alist*)))))
	      (if opposite
		  opposite
		  word)))
	  lst))

(defun test ()
  (and (equal (makeopposites '(THAT IS DIRTY SO I AM SAD))
	      '(THAT ISNT CLEAN SO I AM HAPPY))
       (equal (makeopposites '(IN CHESS BLACK MOVES LAST))
	      '(IN CHESS WHITE MOVES FIRST))
       (equal (makeopposites '(THE TEA IS HOT SO I PUT IT DOWN))
	      '(THE TEA ISNT COLD SO I PUT IT UP))))
