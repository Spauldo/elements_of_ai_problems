;;; chap_02.asd --- System definition file for chapter 2

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

(defpackage :net.spauldo.ai-text.chap-02.system
  (:use :cl :asdf))

(in-package :net.spauldo.ai-text.chap-02.system)

(defsystem "chap_02"
  :name "Chapter 2"
  :author "Jeff Spaulding"
  :license "ISC License"
  :description "Chapter 2 Programming Problems"
  :long-description "Problems from Elements of Artificial Intelligence Chap. 2"
  :depends-on ("utility")
  :components ((:file "cubes"
		      :depends-on ("package"))
	       (:file "double-elements"
		      :depends-on ("package"))
	       (:file "equalelts"
		      :depends-on ("package"))
	       (:file "exify"
		      :depends-on ("package"))
	       (:file "find-next"
		      :depends-on ("package"))
	       (:file "makeopposites"
		      :depends-on ("package"))
	       (:file "palindrome"
		      :depends-on ("package"))
	       (:file "quasi-balancedp"
		      :depends-on ("package" "equalelts"))
	       (:file "replace"
		      :depends-on ("package"))
	       (:file "summation"
		      :depends-on ("package"))
	       (:file "treeutils"
		      :depends-on ("package"))
	       (:file "package")))
