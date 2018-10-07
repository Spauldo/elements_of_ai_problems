;;; cubes.lisp --- Return the first fifteen cubes recursively and iteratively

;;; Copyright (C) 2018 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; Lessons learned:
;;; Not a lot here, other than this is the first time I've used PROG and
;;; go tags.
;;;
;;; I programmed these to start at the largest values and work backward,
;;; because that's how you CONS a list.  Because of this, I had a bug in
;;; the iterative version where I decremented N instead of incrementing it.
;;; If I had used APPEND instead, or just REVERSE, I wouldn't have gotten
;;; confused, but it would have taken the computer many more steps to create
;;; the list.
;;;
;;; Premature optimization, perhaps?  Maybe.
;;;
;;; The recursive function worked right first time, although I'm not satisfied
;;; with it as it's not tail recursive.  Perhaps I'll write a tail-recursive
;;; version later.
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

(defpackage :net.spauldo.ai-text.cubes
  (:use :common-lisp)
  (:export :test))

;;; Write two functions that return a list of the first fifteen cubes.
;;; One function should use recursion, the other should use PROG.

;; Recursive version
(defun cubes-recurse (&optional (max 15) (n 1))
  "Return a list of the cubes between N and MAX."
  (if (<= n max)
      (cons (* n n n) (cubes-recurse max (1+ n)))
      nil))

;; Iterative version
(defun cubes-iterate (&optional (max 15) (n 1))
  "Return a list of the cubes between N and MAX."
  (prog ((cubelist '())
	 (n (1- n)))
   :START
   (cond ((< n max)
	  (let ((this-n (- max n)))
	    (setq cubelist (cons (* this-n this-n this-n) cubelist))))
	 (t (return cubelist)))
   (setq n (1+ n))
   (go :START)))


(defun test ()
  (and (equal (cubes-recurse)
	      '(1 8 27 64 125 216 343 512 729 1000 1331 1728 2197 2744 3375))
       (equal (cubes-iterate)
	      '(1 8 27 64 125 216 343 512 729 1000 1331 1728 2197 2744 3375))))
