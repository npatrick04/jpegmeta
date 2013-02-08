(in-package :jpegmeta)

(proclaim '(optimize (safety 3) (debug 3)))

(defmethod generic-read-byte ((stream pushbackstream))
  (pb-read-byte stream))

(defmethod generic-write-byte (value (stream pushbackstream))
  (pb-read-byte stream))

(define-binary-type raw-bytes (size)
  (:reader (in)
	   (let ((buf (make-array size :element-type '(unsigned-byte 8))))
	     (pb-read-sequence buf in)
	     buf))
  (:writer (out buf)
	   (write-sequence buf out)))

;; JPEG-image
(define-binary-class jpeg-image ()
  ((segments jpeg-segments)))

(defun nil-after-invalid-segment (c)
  (declare (ignore c))
  (let ((restart (find-restart 'nil-after-invalid-segment)))
    (when restart (invoke-restart restart))))

(define-binary-type jpeg-segments ()
  (:reader (in)
	   (loop for segment =
                (restart-case (handler-case (read-value 'jpeg-segment in)
                                (end-of-file () nil))
                  (nil-after-invalid-segment () nil))
	      while segment
	      collect segment))
	      ;;do (format t "~a~%" segment)
  (:writer (out segments)
	   (loop for seg in segments do
		(write-value 'jpeg-segment out seg))))
   
(define-tagged-binary-class jpeg-segment ()
  ((marker jpeg-segment-marker)
   (segment-type u1))
  (:dispatch (find-segment-class segment-type)))

(defmethod print-object ((obj jpeg-segment) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "Jpeg-segment")))

(define-condition segment-marker-off (error)
  ((text :initarg :text :reader text)))

(define-binary-type jpeg-segment-marker ()
  (:reader (in)
	   (let ((b (pb-read-byte in)))
	     (if (/= b #xff) (error 'segment-marker-off :text "Danger! Segment marker is not 0xff!"))
	     b))
  (:writer (out marker)
	   (write-byte marker out)))

(defun find-segment-class (segment-type)
  (let ((seg (cond
	       ((eq segment-type #xc0) 'sof0-segment)
	       ((eq segment-type #xc4) 'dht-segment)
	       ((eq segment-type #xda) 'sos-segment)
	       ((eq segment-type #xdb) 'dqt-segment)
	       ((eq segment-type #xd8) 'soi-segment)
	       ((eq segment-type #xd9) 'eoi-segment)
	       ((eq segment-type #xe0) 'app0-segment)
	       ((eq segment-type #xe1) 'app1-segment)
	       ((eq segment-type #xe2) 'app2-segment)
	       ((eq segment-type #xed) 'app13-segment)
	       ((eq segment-type #xfe) 'com-segment)
	       (t 'generic-segment))))
    ;;(format t "Segment: ~A~%" seg)
    seg))

(define-binary-class generic-segment (jpeg-segment)
  ((size u2)
   (data (raw-bytes :size (- size 2)))))

(defmethod print-object ((obj generic-segment) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (segment-type size) obj
      (format stream "segment-type: ~2,'0X, size: ~D" segment-type size))))

;; SOI-segment
(define-binary-class soi-segment (jpeg-segment) ())

(defmethod print-object ((obj soi-segment) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "'Start of Image'")))

;; EOI-segment 
(define-binary-class eoi-segment (jpeg-segment)
  ((padding end-of-image-padding)))

(defmethod print-object ((obj eoi-segment) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "'End of Image'")))

(define-binary-type end-of-image-padding ()
  (:reader (in)
	   (pb-read-bytes-until in '(#xff #xd8)))
  (:writer (out data)
	   (pb-write-chunks data out)))

;; APP0-segment
(define-binary-class app0-segment (generic-segment) ())

;; APP2-segment
(define-binary-class app2-segment (generic-segment) ())

;; APP13-segment
(define-binary-class app13-segment (jpeg-segment)
  ((size u2)
   (resources (iptc-resources))))

(define-binary-type 8bim-terminated-data ()
  (:reader (in)
	   (let ((data 
		  (pb-read-bytes-until in '(#x38 #x42 #x49 #x4d))))
	     data))
  (:writer (out data)
	   (pb-write-chunks data out)))

(define-binary-type iptc-resources ()
  (:reader (in)
	   (let ((segment-size (- (size (parent-of-type 'app13-segment)) 2))
		 (total-size 0)
		 (resources '()))
	     (loop
		while (< total-size segment-size)
		do (let ((resource (read-value 'iptc-resource in)))
		     (setf resources (append resources (list resource)))
		     (incf total-size (size-of-iptc-resource resource))))
		     ;(format t "resource: ~a : ~a~%"  segment-size total-size)))
	     resources))
  (:writer (out resources)
	   (loop for res in resources do
		(write-value 'iptc-resource out res))))

(define-tagged-binary-class iptc-resource ()
  ((padding 8bim-terminated-data)
   (8bim-bytes u4)
   (resource-id u2)
   (name u2)
   (resource-size u4))
  (:dispatch
   (progn
     ;(format t "8bim: ~4,'0X, resource-id: ~2,'0X, resource-size: ~D~%" 8bim-bytes resource-id resource-size)
     (cond 
       ((= resource-id #x0404) 'iptc-fields-resource)
       (t 'generic-iptc-resource)))))

(define-binary-class generic-iptc-resource (iptc-resource)
  ((data (raw-bytes :size resource-size))))

(define-binary-class iptc-fields-resource (iptc-resource)
  ((fields iptc-fields)
   (end-padding end-of-resource-padding)))

(defun size-of-iptc-resource (res)
  (+ 12 (size-of-chunks (padding res)) (resource-size res)))

(defmethod print-object ((obj iptc-resource) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (padding 8bim-bytes resource-id name resource-size) obj
      (format stream "padding:'~a', 8bim-bytes:~4,'0X, resource-id:~4,'0X, name:~2,'0X, resource-size:~D"
	      padding 8bim-bytes resource-id name resource-size))))

(defmethod print-object ((obj iptc-fields-resource) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (padding 8bim-bytes resource-id name resource-size) obj
      (format stream "padding:~a byte(s), 8bim-bytes:~4,'0X, resource-id:~4,'0X, name:~2,'0X, resource-size:~D"
	      (size-of-chunks padding) 8bim-bytes resource-id name resource-size))))

(define-binary-type end-of-resource-padding ()
  (:reader (in)
	   ; (print (fields (current-binary-object)))
	   (let* ((res (current-binary-object))
		  (size-of-fields (loop for f in (fields res) summing (size-of-iptc-field f)))
		  (bytes-left (- (resource-size res) size-of-fields)))
	     (let ((buf (make-array bytes-left :element-type '(unsigned-byte 8))))
		(pb-read-sequence buf in)
		buf)))
  (:writer (out buf)
	   (write-sequence buf out)))

(define-binary-class p-string ()
  ((size u1)
   (text p-string-text (:size size))))

(define-binary-type p-string-text (size)
  (:reader (in)
	   (let ((padded-size 
		  (- (max (+ size (logand size #x01)) 2) 2)))
	     (with-output-to-string (str)
	       (loop for i from 0 to padded-size do
		    (write-char (read-char in) str)))))
  (:writer (out data)
	   nil))

(define-binary-type iptc-fields ()
  (:reader (in)
	   (let ((resource-size (resource-size (parent-of-type 'iptc-resource)))
		 (total-size 0)
		 (fields '()))
	     (loop
		while (< total-size (- resource-size 4)) ;; adjust for the possibility that the last resource is padded with a few bytes
		do (let ((field (read-value 'iptc-field in)))
		     (setf fields (append fields (list field)))
		     (incf total-size (size-of-iptc-field field))))
		     ; (format t "resource-size: ~a, total-size: ~a~%" resource-size total-size)
		     ; (if field (format t "field: ~a~%" (text field)))))
	     fields))
  (:writer (out fields)
	   (loop for field in fields do
		(write-value 'iptc-field out field))))

(define-binary-class iptc-field ()
  ((marker iptc-marker)
   (tag u1)
   (size u2)
   (text (generic-string :length size :character-type 'iso-8859-1-char))))

(define-binary-type iptc-marker ()
  (:reader (in)
	   (let ((marker (read-value 'u2 in)))
	     (if (/= marker #x1c02)
		 (warn "Warning! IPTC marker is not 1c02!"))
	     marker))
  (:writer (out marker)
	   (write-value 'u2 out marker)))

(defun size-of-iptc-field (field)
  (+ 5 (size field)))

(defmethod print-object ((obj iptc-field) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (marker tag size text) obj
      (format stream "marker:~4,'0X, tag:~2,'0X, size:~a, text:'~a'"
	    marker tag size text))))

(defparameter *iptc-field-names* 
  '((#x00 . :record-version)
    (#x03 . :object-type-reference)
    (#x04 . :object-attribute-reference)
    (#x05 . :object-name)
    (#x07 . :edit-status)
    (#x08 . :editorial-update)
    (#x0a . :urgency)
    (#x0c . :subject-reference)
    (#x0f . :category)
    (#x14 . :supplemental-category)
    (#x16 . :fixture-identifier)
    (#x19 . :keyword)
    (#x1a . :content-location-code)
    (#x1b . :content-location-name)
    (#x1e . :release-date)
    (#x23 . :release-time)
    (#x25 . :expiration-date)
    (#x26 . :expiration-time)
    (#x28 . :special-instructions)
    (#x2a . :action-advised)
    (#x2d . :reference-service)
    (#x2f . :reference-date)
    (#x32 . :reference-number)
    (#x37 . :date-created)
    (#x3c . :time-created)
    (#x3e . :digital-creation-date)
    (#x3f . :digital-creation-time)
    (#x41 . :originating-program)
    (#x46 . :program-version)
    (#x4b . :object-cycle)
    (#x50 . :byline)
    (#x55 . :byline-title)
    (#x5a . :city)
    (#x5c . :sub-location)
    (#x5f . :province-state)
    (#x64 . :country-primary-location-code)
    (#x65 . :country-primary-location-name)
    (#x67 . :original-transmission-reference)
    (#x69 . :headline)
    (#x6e . :credit)
    (#x73 . :source)
    (#x74 . :copyright-notice)
    (#x76 . :contact)
    (#x78 . :caption-abstract)
    (#x7a . :writer-editor)
    (#x7d . :rasterized-caption)
    (#x82 . :image-type)
    (#x83 . :image-orientation)
    (#x87 . :language-identifier)
    (#x96 . :audio-type)
    (#x97 . :audio-sampling-rate)
    (#x98 . :audio-sampling-resolution)
    (#x99 . :audio-duration)
    (#x9a . :audio-outcue)
    (#xc8 . :object-data-preview-file-format)
    (#xc9 . :object-data-preview-file-format-version)
    (#xca . :object-data-preview-data)))

(defun iptc-tag->name (tag)
  (cdr (assoc tag *iptc-field-names*)))

(defun iptc-name->tag (name)
  (car (rassoc name *iptc-field-names*)))

;; COM-segment
(define-binary-class com-segment (generic-segment) ())

;; DHT-segment
(define-binary-class dht-segment (generic-segment) ())

;; DQT-segment
(define-binary-class dqt-segment (generic-segment) ())

;; SOF0-segment
(define-binary-class sof0-segment (generic-segment) ())

;; SOS-segment
(define-binary-class sos-segment (jpeg-segment)
  ((size u2)
   (padding u3)
   (image-data eoi-terminated-data)))

(define-binary-type eoi-terminated-data ()
  (:reader (in)
	   (pb-read-bytes-until in '(#xff #xd9)))
  (:writer (out data)
	   (pb-write-chunks data out)))

;; Utility functions
(defun alist-combine-keys (alist)
  (labels
      ((alist-add-value (alist key value)
	 (cond
	   ((not alist) 
	    (cons (cons key value) nil))
	   ((eq (caar alist) key)
	    (cons (cons key (append
			     (if (consp (cdar alist)) (cdar alist)
				 (list (cdar alist)))
			     (list value)))
		  (cdr alist)))
	   (t (cons (car alist)
		    (alist-add-value (cdr alist) key value)))))
       (alist-loop (alist res)
	 (if alist (alist-loop (cdr alist) (alist-add-value res (caar alist) (cdar alist)))
	     res)))
    (alist-loop alist '())))

(defun find-resource (segment resource-type)
  (car (remove-if-not
	#'(lambda (res) (typep res resource-type))
	(resources segment))))

(defun find-segment (jpeg-image segment-type)
  (car (remove-if-not 
	#'(lambda (seg) (typep seg segment-type))
	(segments jpeg-image))))

(defun make-iptc-fields-resource (iptc-fields)
  "Create a new instance of iptc-fields-resource with default values for
   most slots."
  (let* ((size (loop for f in iptc-fields sum (size-of-iptc-field f)))
	 (even-size (if (oddp size) (+ 1 size) size))
	 (8bim-padding (make-instance 'chunk-vector)))
    (loop for b in '(#x50 #x68 #x6f #x74 #x6f #x73 #x68 #x6f #x70 #x20 #x33 #x2e #x30 #x00)
       do (chunk-vector-push b 8bim-padding))
    (make-instance 'iptc-fields-resource
		   :padding 8bim-padding
		   :8bim-bytes '#x3842494d
		   :resource-id #x0404
		   :name #x0000
		   :resource-size even-size
		   :fields iptc-fields
		   :end-padding (if (/= size even-size) '(#x00) '()))))

(defun ensure-segment-has-iptc-fields-resource (app13-segment iptc-fields)
  (labels ((resource-iter (resources)
	     (cond ((not resources) (list (make-iptc-fields-resource iptc-fields)))
		   ((typep (car resources) 'iptc-fields-resource)
		    (cons (make-iptc-fields-resource iptc-fields) (cdr resources)))
		   (t (cons (car resources)
			    (resource-iter (cdr resources)))))))
    (let* ((res (resource-iter (resources app13-segment)))
	   (size (loop for r in res sum (size-of-iptc-resource r))))
      (make-instance 'app13-segment
		     :marker #xff
		     :segment-type #xed
		     :size (+ size 2)
		     :resources res))))

(defun ensure-image-has-iptc-fields-resource (jpeg-image iptc-fields)
  (labels ((segment-iter (segments)
	     (cond ((not segments) '())
		   ((typep (car segments) 'app13-segment)
		    (cons (ensure-segment-has-iptc-fields-resource (car segments) iptc-fields)
			  (cdr segments)))
		   ((typep (car segments) 'sos-segment)
		    (append (list 
			     (ensure-segment-has-iptc-fields-resource 
			      (make-instance 'app13-segment
					     :resources '())
			      iptc-fields))
			    segments))
		   (t (cons (car segments)
			    (segment-iter (cdr segments)))))))
    (make-instance 'jpeg-image 
		   :segments (segment-iter (segments jpeg-image)))))


(defun normalize-list (arg)
  (if (listp arg) 
      arg
      (list arg)))

(defun make-iptc (tuple)
  (let ((tag (car tuple)))
    (mapcar
     #'(lambda (text)
	 (make-instance 'iptc-field
			:marker #x1c02
			:tag (if (numberp tag) tag (iptc-name->tag tag))
			:size (length text)
			:text text))
     (normalize-list (cdr tuple)))))

;; Main interface functions
(defun read-jpeg-from-stream (stream)
  "Read a JPEG from a stream, parse the contents and return a represe"
  (let ((pb-stream (make-instance 'pushbackstream :stream stream)))
    (read-value 'jpeg-image pb-stream)))

(defun read-jpeg-from-file (file-name)
  "Slurp a JPEG file, parse the contents and return a representation of that data."
  (with-open-file (in file-name :element-type '(unsigned-byte 8))
    (read-jpeg-from-stream in)))

(defun write-jpeg-to-stream (jpeg-image stream)
  (write-value 'jpeg-image stream jpeg-image))

(defun write-jpeg-to-file (jpeg-image file-name)
  "Write the given JPEG data structure to a file."
  (with-open-file (out file-name
		       :direction :output
		       :if-exists :supersede
		       :element-type '(unsigned-byte 8))
    (write-jpeg-to-stream jpeg-image out)))

(defun get-iptc-fields (jpeg-image)
  "Extract the IPTC fields from a JPEG data structure."
  (alist-combine-keys
   (mapcar 
    (lambda (x)
      (cons (or (iptc-tag->name (tag x))
		(tag x))
	    (text x)))
    (car (mapcar #'(lambda (x) (fields x))
		 (remove-if-not #'(lambda (x) (typep x 'iptc-fields-resource))
				(car (mapcar #'(lambda (x) (resources x))
					     (remove-if-not #'(lambda (x) (typep x 'app13-segment)) 
							    (segments jpeg-image))))))))))

(defun get-iptc-field (jpeg-image field)
  (cdr (assoc field (get-iptc-fields jpeg-image))))

(defun set-iptc-fields (jpeg-image iptc-fields)
  "Set the IPTC fields of a JPEG, returning a new image data structure."
  (ensure-image-has-iptc-fields-resource jpeg-image
					 (mapcan #'make-iptc iptc-fields)))
					 