(in-package :cl-user)

(defpackage :jpegmeta
  (:use :com.gigamonkeys.binary-data :common-lisp :asdf :pushbackstreams
	:com.gigamonkeys.binary-data.common-datatypes)
  (:shadowing-import-from :alexandria if-let when-let appendf)
  (:export :read-jpeg-from-stream
	   :read-jpeg-from-file
	   :write-jpeg-to-stream
	   :write-jpeg-to-file
	   :get-iptc-fields
	   :get-iptc-field
	   :set-iptc-fields
	   :get-tiff-fields
	   :get-tiff-field
	   :get-exif-fields
	   :get-exif-field
	   :get-gps-fields
	   :get-ifds

           ;; Restarts
           :nil-after-invalid-segment
           :return-zero))

