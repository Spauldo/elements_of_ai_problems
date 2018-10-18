;;; equalelts.lisp --- Determine if all elements in a list are EQUAL

;;; Copyright (C) 2018 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; Lessons learned:
;;; This one was harder than I first supposed, mostly because I wanted to make
;;; it proof against corner cases (like atoms, improper lists, lists of NIL,
;;; etc.).
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

(in-package :net.spauldo.ai-text.equalelts)

(defun equalelts (lst)
  "Return T if all elements of LST are EQUAL."
  (cond
    ;; LST is an atom
    ((atom lst)
     T)
    ;; LST contains only NIL
    ((equal lst '(NIL . NIL))
     T)
    ;; LST is not a list
    ((not (listp lst))
     (error "LST is not a list: ~s" LST))
    ;; LST is an improper list
    ((not (listp (cdr lst)))
     (equal (car lst) (cdr lst)))
    ;; LST contains a single element
    ((not (cdr lst))
     T)
    ;; The first two elements of LST are EQUAL
    ((equal (car lst)
	    (cadr lst))
     (equalelts (cdr lst)))
    ;; Any other case
    (T
     NIL)))

(defun test ()
  (and (equalelts '(a a a a a))
       (equalelts '((b c) (b c) (b c) (b c)))
       (equalelts '(a . a))
       (equalelts '(a))
       (equalelts 'a)
       (equalelts '(nil))
       (equalelts '(nil nil nil nil))
       (not (equalelts '(a a a a b)))
       (not (equalelts '((b c) (c d) (d e) (e f))))
       (not (equalelts '(a . b)))))
