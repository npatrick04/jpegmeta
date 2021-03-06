(defpackage :jpegmeta (:use :asdf :cl))

(in-package :jpegmeta)

(defsystem jpegmeta
  :name "jpegmeta"
  :author "Fredrik Appelberg <fredrik.appelberg@gmail.com>"
  :version "0.1"
  :maintainer "Fredrik Appelberg <fredrik.appelberg@gmail.com>"
  :licence "BSD"
  :description "JPEG metadata manipulation."
  :long-description ""
  :depends-on ("com.gigamonkeys.binary-data" "pushbackstreams" "alexandria"
               "ieee-floats")
  :components
  ((:file "package")
   (:file "jpegmeta" :depends-on ("package"))))
        
