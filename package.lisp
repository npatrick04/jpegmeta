(in-package :cl-user)

(defpackage :jpegmeta
  (:use :com.gigamonkeys.binary-data :common-lisp :asdf :pushbackstreams
	:com.gigamonkeys.binary-data.common-datatypes)
  (:export :read-jpeg-from-stream
	   :read-jpeg-from-file
	   :write-jpeg-to-stream
	   :write-jpeg-to-file
	   :get-iptc-fields
	   :get-iptc-field
	   :set-iptc-fields))

