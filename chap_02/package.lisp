;;; package.lisp --- Chapter 2 package definitions

;;; Copyright (C) 2018 Jeff Spaulding <sarnet@gmail.com>
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

(in-package :cl-user)

(defpackage :net.spauldo.ai-text.cubes
  (:use :common-lisp)
  (:export :test))

(defpackage :net.spauldo.ai-text.double-elements
  (:use :common-lisp)
  (:export :test))

(defpackage :net.spauldo.ai-text.equalelts
  (:use :common-lisp)
  (:export :test :equalelts))

(defpackage :net.spauldo.ai-text.exify
  (:use :common-lisp)
  (:export :test))

(defpackage :net.spauldo.ai-text.find-next
  (:use :common-lisp)
  (:export :test))

(defpackage :net.spauldo.ai-text.palindrome
  (:use :common-lisp)
  (:export :test))

(defpackage :net.spauldo.ai-text.quasi-balancedp
  (:use :common-lisp :net.spauldo.ai-text.equalelts)
  (:export :test))

(defpackage :net.spauldo.ai-text.replace
  (:use :common-lisp)
  (:export :test))

(defpackage :net.spauldo.ai-text.summation
  (:use :common-lisp)
  (:export :test))

(defpackage :net.spauldo.ai-text.treeutils
  (:use :common-lisp)
  (:export :test))

(defpackage :net.spauldo.ai-text.makeopposites
  (:use :common-lisp)
  (:export :test))
