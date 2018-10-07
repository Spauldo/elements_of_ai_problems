;;; palindrome.lisp --- Test if a list is the same backward and forward

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

(defpackage :net.spauldo.ai-text.palindrome
  (:use :common-lisp)
  (:export test))

(defun palindromep (lst)
  "Return T if LST is the same with all top-level elements reversed.
We assume LST is a proper list."
  ;; Yes, we could just use one of the built-in reversing functions that are
  ;; in Common Lisp, but that kind of goes against the spirit of the thing.
  (let ((rev-lst '()))
    (dolist (elt lst)
      (setq rev-lst (cons elt rev-lst)))
    (equal lst rev-lst)))

(defun test ()
  (and (palindromep '(1 2 1))
       (palindromep '(1 2 (1 2) 3 (1 2) 2 1))
       (palindromep '(1 1))
       (palindromep '(1))
       (not (palindromep '(1 2 3)))
       (not (palindromep '(1 2 (1 2) 3 (1 1) 2 1)))
       (not (palindromep '(1 2)))))
