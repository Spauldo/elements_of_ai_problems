;;; utility.asd --- System definition file for Elements of AI utilities

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

(defpackage :net.spauldo.ai-text.utility.system
  (:use :cl :asdf))

(in-package :net.spauldo.ai-text.utility.system)

(defsystem "ai-text.utility"
    :name "Utility functions"
    :author "Jeff Spaulding"
    :license "ISC License"
    :description "Utility functions for Elements of AI"
    :long-description "Utility functions for Elements of AI"
    :components ((:file "utility"
			:depends-on ("package"))
		 (:file "package")))
