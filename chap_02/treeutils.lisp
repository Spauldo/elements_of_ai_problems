;;; treeutils.lisp --- Question 17 from Elements of Artificial Intelligence

;;; Copyright (C) 2018 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; Lessons learned:
;;;
;;; One of the things about LISP that's pretty ingrained is to avoid repetition
;;; at all costs.  The question asks for two functions that call each other.
;;; However, both functions would be nearly identical, and that irked me.  So
;;; this really isn't a solution in the sense that it fulfills the requirements
;;; of the question; it gets the right answer, but it doesn't do it the way
;;; the book wants.
;;;
;;; I've chosen to go ahead and do it this way because I've written plenty of
;;; mutually recursive functions in the past, so I'm not really learning much
;;; by doing that.  On the other hand, coming from Scheme, I haven't used
;;; funcall that much (you don't need it in Scheme).  Plus, I want to solve
;;; these problems in as "lispy" a way as possible.
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

(in-package :net.spauldo.ai-text.treeutils)

(defun maxnum (a b)
  "Return A or B, whichever is largest."
  (if (> a b)
      a
      b))

(defun minnum (a b)
  "Return A or B, whichever is smallest."
  (if (< a b)
      a
      b))

(defun tree-compare (lst operator)
  "Alternates returning the maximum and minimum values of the tree LST.
OPERATOR must be one of 'MAXNUM or 'MINNUM."
  (let ((other-operator (if (eq operator 'maxnum) 'minnum 'maxnum)))
    (cond ((numberp lst)
	   lst)
	  ((not lst)
	   (error "NIL found in tree!"))
	  ((listp LST)
	   (if (= 2 (list-length LST))
	       (funcall operator
			(tree-compare (car lst) other-operator)
			(tree-compare (cadr lst) other-operator))
	       (error "Tree not binary!")))
	  (T
	   (error "LST not a list or number: ~a" lst)))))

(defun treemax (lst)
  "Starts TREE-COMPARE on LST with MAXNUM."
  (tree-compare lst 'maxnum))

(defun treemin (lst)
  "Starts TREE-COMPARE on LST with MINNUM"
  (tree-compare lst 'minnum))

(defun test ()
  (and (= 3 (treemax '((3 (2 5)) (7 (3 1)))))
       (= 6 (treemax '(((1 2) (3 4)) ((5 (6 7)) 8))))
       (= 5 (treemax '(1 (8 (2 (7 (3 (6 (4 5)))))))))))
