;;; quasi-balancedp.lisp --- Determine if a list is "quasi-balanced"
;;;
;;; Copyright (C) 2018 Jeff Spaulding <sarnet@gmail.com>
;;;
;;;
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

(defun quasi-balancedp (lst)
  (cond )
  (listp lst))

(defun test ()
  (and (quasi-balancedp '((A B) (C D) (E F)))
       (not (quasi-balancedp '((A (B C)) (D E) (F G))))))
