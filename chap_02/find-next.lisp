;;; find-next.lisp --- Find the next element in a sequence

;;; Copyright (C) 2018 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; Given the first four elements of an arithmetic or geometric sequence,
;;; return the next element.
;;;
;;; Lessons learned:
;;;
;;; I did a few iterations of this one.  I wanted to walk the sequence
;;; exactly once, which complicated things significantly.  The first version
;;; walked the sequence twice before it even started to look for sequences.
;;;
;;; You can't nest DEFUN!  I remember reading about that when I was skimming
;;; the chapter on special operators in Practical Common Lisp, but I've never
;;; had call to do so until now.  My second iteration of this used a nested
;;; DEFUN and I kept getting warnings every time I'd run my test function.  It
;;; would work, but it's obviously wrong.
;;;
;;; In Scheme, it's quite normal to nest define macros as helper functions.
;;;
;;; MAP-PRED seems useful.  I imagine that there's an equivalent function that's
;;; already part of Common Lisp, but I didn't look for one.  I didn't end up
;;; using it in the last version of this function, but I'm keeping it here for
;;; now.
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

(in-package :net.spauldo.ai-text.find-next)

(defvar unknown T
  "The symbol returned when the next element of a sequence can't be found.")

(defun find-next (seq)
  "Find the next element of arithmetic or geometric sequence SEQ."

  (labels ((fn-helper (seq arith-dist multi-ratio)
	     "Find the next element of arithmetic or geometric series SEQ.
The first element of SEQ is assumed to be numeric. Use T as initial arguments
for ARITH-DIST and MULTI-RATIO, unless first element of SEQ is zero in which
case set MULTI-RATIO to NIL."
	     (cond
	       ;; The second element of SEQ must be a number if it exists:
	       ((and (cdr seq)
		     (not (numberp (cadr seq))))
		(error "SEQ must contain all numbers."))

	       ;; Only one element of SEQ exists, and ARITH-DIST is a number:
	       ((and (numberp arith-dist)
		     (not (cdr seq)))
		(+ (car seq) arith-dist))

	       ;; Only one element of SEQ exists, and MULTI-RATIO is a number:
	       ((and (numberp multi-ratio)
		     (not (cdr seq)))
		(* (car seq) multi-ratio))

	       ;; Only one element of SEQ exists
	       ((not (cdr seq))
		'UNKNOWN)

	       ;; ARITH-DIST is set to T (i.e. first run of this function):
	       ((eq ARITH-DIST T)
		(setq ARITH-DIST (- (cadr seq) (car seq)))
		;; Geometric functions cannot contain zeroes by definition.
		(if (and (not (zerop (car seq)))
			 (not (zerop (cadr seq))))
		    (setq multi-ratio (/ (cadr seq) (car seq)))
		    (setq multi-ratio NIL))
		(fn-helper (cdr seq) arith-dist multi-ratio))

	       ;; One or both of ARITH-DIST and MULTI-RATIO are numeric
	       ((or (numberp arith-dist)
		    (numberp multi-ratio))
		(unless (and arith-dist
			     (= arith-dist (- (cadr seq) (car seq))))
		  (setq arith-dist NIL))
		(unless (and multi-ratio
			     (= multi-ratio (/ (cadr seq) (car seq))))
		  (setq multi-ratio NIL))
		(fn-helper (cdr seq) arith-dist multi-ratio))

	       ;; Neither ARITH-DIST nor MULTI-RATIO are numeric
	       (T
		'UNKNOWN))))

    ;; We have to validate the first element of SEQ before calling our helper
    ;; function.
    (unless (numberp (car seq))
      (error "SEQ must contain all numbers."))

    ;; If the first element of SEQ is zero, call our helper function with
    ;; MULTI-RATIO set to NIL.  Otherwise, call it with MULTI-RATIO set to T.
    (if (zerop (car seq))
	(fn-helper seq T NIL)
	(fn-helper seq T T))))

(defun map-pred (pred lst)
  "Return T if every item in LST passes predicate PRED."
  (cond ((not (listp lst)) ; Doesn't make sense on non-lists
	 (error "LST not a list: ~a" lst))
	((not lst) ; Doesn't make sense on the empty list
	 NIL)
	((not (funcall pred (car lst))) ; Fail if CAR of LST fails predicate
	 NIL)
	((cdr lst) ; Recurse if there are more items in the list
	 (map-pred pred (cdr lst)))
	(T T))) ; End of the list, return T

(defun test ()
  (and (= 10 (find-next '(2 4 6 8)))
       (= 324 (find-next '(4 -12 36 -108)))
       (eq 'UNKNOWN (find-next '(3 1 4 1)))
       (= 5 (find-next '(1 2 3 4)))
       (= 2 (find-next '(10 8 6 4)))
       (= 32 (find-next '(2 4 8 16)))
       (= 1 (find-next '(16 8 4 2)))
       (eq 'UNKNOWN (find-next '(1 3 2 4)))))
