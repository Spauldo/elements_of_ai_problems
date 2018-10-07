;;; summation.lisp --- Use APPLY to sum a list of numbers

;;; Copyright (C) 2018 Jeff Spaulding <sarnet@gmail.com>
;;;
;;; Lessons Learned:
;;; Not much, this is pretty basic.
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

(defpackage :net.spauldo.ai-text.summation
  (:use :common-lisp))

(defun sum (item)
  "Sum all elements in the list ITEM.
We are assuming that ITEM is a list of objects than can be summed."
  (apply #'+ ITEM))

(defun test ()
  (let ((ilist '(3 7 11 13)))
    (= (sum ilist)
       34)))
