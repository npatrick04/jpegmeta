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

;; APP1-segment
(define-tagged-binary-class app1-segment (jpeg-segment)
  ((size u2)
   (exif (iso-8859-1-string :length 4))
   (exif-zeros u2))
  (:dispatch (cond ((string= (string-upcase exif) "EXIF") 'exif-app1-segment)
                   (t 'unknown-app1-segment))))

(define-binary-class unknown-app1-segment (app1-segment)
  ((data (raw-bytes :size (- size 2 4 2)))))

(defmethod print-object ((obj unknown-app1-segment) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (exif) obj
      (format stream "App1 segment tag=~A" exif))))

(define-binary-class exif-app1-segment (app1-segment)
  ((endian tiff-endian
           :post-read (setf monkey-types:*endianness* endian)
           :post-write (setf monkey-types:*endianness* endian))
   (fourty-two u2)
   (ifd (ifd-pointer :type 'tiff-ifd
                     :add-to-read nil))
   (pointer-space (ifd-pointer-space :offset 8
                                     :initial-pointer ifd
                                     :end (- size 2 4 2))
                  :post-read (setf monkey-types:*endianness* :big-endian)
                  :post-write (setf monkey-types:*endianness* :big-endian))))

(defmethod print-object ((obj exif-app1-segment) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (endian fourty-two) obj
      (format stream "endianness:~A, fourty-two:~A"
	      endian fourty-two))))

(define-binary-class ifd-pointer-type ()
  ((offset u4)
   (value-type (optional))
   (value (optional))
   (entry-count (optional))))

(defmethod print-object ((obj ifd-pointer-type) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (offset value-type value) obj
      (format stream "type:~A at offset:~A ~A" value-type offset
	      (if value
		  (format nil "evaluated to ~A" value)
		  "unevaluated")))))
	     
(define-binary-type ifd-pointer (type (add-to-read t))
  (:reader (in)
	   (let ((pointer (read-value 'ifd-pointer-type in)))
	     (declare (special to-be-read))
	     (setf (value-type pointer) type)
	     (if (zerop (offset pointer))
		 (progn (setf (value pointer) nil)
			pointer)
		 (progn (when add-to-read (push pointer to-be-read))
			pointer))))
  (:writer (out data)
           (declare (ignorable out data))
           (error "No writer defined for ifd-pointer")
	   ;; Do something
	   nil))

(define-condition invalid-ifd-type (error)
  ((ifd-type :initarg :type :reader ifd-type)))

(defun return-zero (c)
  (declare (ignore c))
  (let ((restart (find-restart 'return-zero)))
    (when restart (invoke-restart restart))))

(defun get-type-size (type)
  (cond
    ((member type '(u1 s1-o iso-8859-1-string undefined-byte null-terminated-string)) 1)
    ((member type '(u2 s2-o u2-o)) 2)
    ((member type '(u4 s4-o u4-o single-float)) 4)
    ((member type '(double-float rational srational)) 8)
    (t (restart-case (error 'invalid-ifd-type :ifd-type type)
	 (return-zero () 0)))))

(define-binary-type maybe-pointer (type entry-count)
  (:reader (in)
	   (handler-bind ((invalid-ifd-type #'return-zero))
	     (let ((type-size (get-type-size (if (listp type)
						 (car type)
						 type))))
	       ;; (format
	       ;; 	       (id (current-binary-object)) type entry-count type-size)
	       (cond
		 ((zerop type-size) (list 'dunno
					  (id (current-binary-object))
					  type
					  entry-count
					  type-size))
		 ((> (* entry-count type-size) 4)
		  (read-value 'ifd-pointer in :type type :add-to-read t))
		 ((member (id (current-binary-object)) special-ifd-tags)
		  (read-value 'ifd-pointer in :type 'tiff-ifd))
		 (t (progn
		      (let ((final-value '()))
			(dotimes (i entry-count)
			  (appendf final-value (list (eval (type->read-value type in)))))
			(let ((ignored-data (read-value 'raw-bytes in :size (- 4 (* entry-count type-size)))))
                          (declare (ignore ignored-data))
			  ;; (format t "Final value=~A~%" final-value)
			  ;; (format t "Size of final value: ~A~%Size of ignored data: ~A~%"
			  ;; 	  (length final-value) (length ignored-data))
			  final-value))))))))
  (:writer (out data)
           (declare (ignorable out data))
           (error "No writer defined for maybe-pointer")
	   ;; TODO something
	   ))

(define-binary-type ifd-pointer-space (offset initial-pointer end)
  (:reader (in)
	   ;; (format t "App1 segment size:~A~%end offset:~A~%"
	   ;; 	   (size (parent-of-type 'app1-segment))
	   ;; 	   end)
	   (let ((to-be-read (list initial-pointer (make-instance
						    'ifd-pointer-type
						      :offset end
						      :value-type :the-end-of-ifd-space
						      :value 'unevaluated)))
		 (start (- (pb-stream-position in) offset))
		 (unread-chunks '())
                 (next-ptr nil))
	     (declare (special to-be-read))
	     (loop while to-be-read
		do (progn
                     (setf next-ptr (pop to-be-read))
		     (let ((current-offset (- (pb-stream-position in) start)))
		       ;; (format t "Next to-be-read while at offset ~A: ~A~%" current-offset next-ptr)
		       (when (>= (offset next-ptr) current-offset)
			 (if-let (chunk (read-next-pointer-value next-ptr in current-offset))
			   (push chunk unread-chunks))
			 (setf to-be-read (sort to-be-read #'< :key #'offset)))))
                until (eq (value-type next-ptr)
                            :the-end-of-ifd-space))
	     ;; (format t "Finished with pointer space, at offset ~A~%"
	     ;; 	     (- (pb-stream-position in) start))
	     (nreverse unread-chunks)))
  (:writer (out data)
           (error "Fix writer for ifd-pointer-space")
	   (write-value 'ifd-pointer out data)))

(defun has-length (type)
  (member type '(iso-8859-1-string)))

(defclass chunk-type ()
  ((tiff-offset :accessor tiff-offset
		:initarg :tiff-offset
		:initform (error "Chunk-type requires a tiff offset"))
   (contents :accessor chunk-contents
	     :initarg :chunk-contents
	     :initform (error "Chunk-type requires contents"))))

(defmethod print-object ((obj chunk-type) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (tiff-offset contents) obj
      (format stream "offset:~A, size:~A" tiff-offset (length contents)))))

(defun read-next-pointer-value (pt in offset)
  "returns unread chunks"
  (let ((unread-chunk nil)
        (new-pointer-value nil))
    ;; (format t "Reading next pointer value, type:~A~%" (value-type pt))
    ;; (format t "  Current offset: ~A, pointer offset: ~A~%" offset (offset pt))
    (unless (equal (offset pt) offset)
      (setf unread-chunk
	    (make-instance 'chunk-type
			   :tiff-offset offset
			   :chunk-contents (read-value 'raw-bytes in :size (- (offset pt) offset)))))
    (incf offset (- (offset pt) offset))
    ;; (format t "Now reading pointer value at ~A: ~%" offset)
    ;; (setf new-pointer-value (type->read-value (value-type pt) in))
    ;; (format t "~A~%") 
    ;; (format t "entry-count=~A~%" (entry-count pt))
    (setf (value pt)
	  (cond
	    ((eq (value-type pt) :the-end-of-ifd-space) nil)
	    ((null (entry-count pt)) (eval (type->read-value (value-type pt) in)))
	    (t (let ((num-bytes (* (entry-count pt)
				   (get-type-size (if (listp (value-type pt))
						      (car (value-type pt))
						      (value-type pt)))))
		     (start-pos (pb-stream-position in)))
		 ;; (format t "num-bytes=~A~%" num-bytes)
		 (loop while (< (pb-stream-position in)
				(+ start-pos num-bytes))
		    collect (eval (type->read-value (value-type pt) in)))))))
    ;; (format t "PT=~A~%" pt)
    unread-chunk))

(define-binary-class tiff-ifd ()
  ((num-entries u2)
   (entries (ifd-entries :count num-entries))
   (next-ifd (ifd-pointer :type 'tiff-ifd))))

(define-binary-type ifd-entries (count)
  (:reader (in)
	   ;; (format t "Reading ifd-entries, count=~A~%" (num-entries (current-binary-object)))
	   (let ((result '()))
	     (dotimes (i (num-entries (current-binary-object)) (nreverse result))
	       (handler-bind ((com.gigamonkeys.binary-data::no-symbol-for-value-enum #'com.gigamonkeys.binary-data::return-unknown-value))
		 (let ((latest-entry (read-value 'ifd-entry in)))
		   (push latest-entry result))))))
  (:writer (out data)
	   (dolist (entry data)
	     (write-value 'ifd-entry out entry))))

(define-binary-class ifd-entry ()
  ((id (ifd-tag :type 'u2))
   (entry-type (ifd-type :type 'u2))
   (entry-count u4)
   (pointer (maybe-pointer :type entry-type
			   :entry-count entry-count))))

(defmethod print-object ((obj ifd-entry) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (id entry-type entry-count pointer) obj
      (format stream "id:~A, entry-type:~A, entry-count:~A, value:~A"
	      id (if (listp entry-type)
		     (car entry-type)
		     entry-type)
	      entry-count
	      pointer))))

;;;
;;;  Exif/Tiff basic types
;;; 

(define-enumeration ifd-tag
    (gps-version-id                     0)
  (gps-latitude-ref                     1)
  (gps-latitude				2)
  (gps-longitude-ref			3)
  (gps-longitude			4)
  (gps-altitude-ref			5)
  (gps-altitude				6)
  (gps-timestamp			7)
  (gps-satellites			8)
  (gps-status				9)
  (gps-measure-mode			10)
  (gps-degree-of-precision		11)
  (gps-speed-ref			12)
  (gps-speed				13)
  (gps-track-ref			14)
  (gps-track				15)
  (gps-img-direction-ref		16)
  (gps-img-direction			17)
  (gps-map-datum			18)
  (gps-dest-latitude-ref		19)
  (gps-dest-latitude			20)
  (gps-dest-longitude-ref		21)
  (gps-dest-longitude			22)
  (gps-dest-bearing-ref			23)
  (gps-dest-bearing			24)
  (gps-dest-distance-ref		25)
  (gps-dest-distance			26)
  (gps-processing-method		27)
  (gps-area-information			28)
  (gps-datestamp			29)
  (gps-differential			30)
  (photometric-interpretation		262)
  (compression				259)
  (image-length				257)
  (image-width				256)
  ;; (resolution-unit			259)
  (min-sample-value			280)
  (max-sample-value			281)
  (x-resolution				282)
  (y-resolution				283)
  (date-time				306)
  (rows-per-strip			278)
  (make					271)
  (model				272)
  (strip-offsets			273)
  (orientation				274)
  (strip-byte-counts			279)
  (resolution-unit			296)
  (JPEG-Interchange-Format		513)
  (JPEG-Interchange-Format-Length	514)
  (YCbCr-Positioning			531)
  (private-exif-ifd			34665)
  (gps-info				34853)
  (exposure-time			33434)
  (f-number				33437)
  (exposure-program			34850)
  (spectral-sensitivity			34852)
  (iso-speed-ratings			34855)
  (oecf					34856)
  (exif-version				36864)
  (date-time-original			36867)
  (date-time-digitized			36868)
  (components-configuration		37121)
  (compressed-bits-per-pixel		37122)
  (shutter-speed-value			37377)
  (aperture-value			37378)
  (brightness-value			37379)
  (exposure-bias-value			37380)
  (max-aperture-value			37381)
  (subject-distance			37382)
  (metering-mode			37383)
  (light-source				37384)
  (flash				37385)
  (focal-length				37386)
  (subject-area				37396)
  (maker-note				37500)
  (user-comment				37510)
  (subsec-time				37520)
  (subsec-time-original			37521)
  (subsec-time-digitized		37522)
  (flashpix-version			40960)
  (color-space				40961)
  (pixel-x-dimension			40962)
  (pixel-y-dimension			40963)
  (related-sound-file			40964)
  (interoperability-ifd                 40965)
  (flash-energy				41483)
  (spatial-frequency-response		41484)
  (focal-plane-x-resolution		41486)
  (focal-plane-y-resolution		41487)
  (focal-plane-resolution-unit		41488)
  (subject-location			41492)
  (exposure-index			41493)
  (sensing-method			41495)
  (file-source				41728)
  (scene-type				41729)
  (cfa-pattern				41730)
  (custom-rendered			41985)
  (exposure-mode			41986)
  (white-balance			41987)
  (digital-zoom-ratio			41988)
  (focal-length-in-35mm-film            41989)
  (scene-capture-type			41990)
  (gain-control				41991)
  (contrast				41992)
  (saturation				41993)
  (sharpness				41994)
  (device-setting-description		41995)
  (subject-distance-range		41996)
  (image-unique-id			42016))

(defparameter special-ifd-tags '(private-exif-ifd
				 gps-info
				 interoperability-ifd))

(define-binary-type null-terminated-string ()
  (iso-8859-1-terminated-string :terminator +null+))

(define-enumeration ifd-type 
  (u1           1)
  (null-terminated-string 2)
  (u2		3)
  (u4		4)
  (rational     5)
  (s1		6)
  (undefined-byte 7)
  (s2		8)
  (s4		9)
  (srational    10)
  (single-float	11)
  (double-float  12))

(define-binary-type single-float ()
  (:reader (in)
           (ieee-floats:decode-float32 (read-value 'u4 in)))
  (:writer (out data)
           (write-value 'u4 out (ieee-floats:encode-float32 data))))

(define-binary-type double-float ()
  (:reader (in)
           (ieee-floats:decode-float64 (read-value 'u8 in)))
  (:writer (out data)
           (write-value 'u8 out (ieee-floats:encode-float64 data))))

(define-binary-type undefined-byte () (unsigned-integer :bytes 1 :bits-per-byte 8))

(define-binary-type rational ()
  (:reader (in)
	   (handler-case
               (/ (read-value 'u4 in)
                  (read-value 'u4 in))
	     (division-by-zero () 0)))
  (:writer (out data)
           (write-value 'u4 out (numerator data))
           (write-value 'u4 out (denominator data))))

(define-binary-type srational ()
  (:reader (in)
	   (handler-case
               (/ (read-value 's4 in)
                  (read-value 's4 in))
	     (division-by-zero () 0)))
  (:writer (out data)
           (write-value 's4 out (numerator data))
           (write-value 's4 out (denominator data))))

(define-binary-type tiff-endian ()
  (:reader (in)
	   (let ((chars (read-value 'iso-8859-1-string in :length 2)))
	     (cond
	       ((equal chars "II") :little-endian)
	       ((equal chars "MM") :big-endian)
	       (t (error "Danger: invalid endianness in TIFF header~%Exif=~A" (exif (parent-of-type 'app1-segment)))))))
  (:writer (out data)
	   (if (equal data :big-endian)
	       (pb-write-chunks "MM" out)
	       (pb-write-chunks "II" out))))

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
   (text (p-string-text :size size))))

(define-binary-type p-string-text (size)
  (:reader (in)
	   (let ((padded-size 
		  (- (max (+ size (logand size #x01)) 2) 2)))
	     (with-output-to-string (str)
	       (loop for i from 0 to padded-size do
		    (write-char (read-char in) str)))))
  (:writer (out data)
           (declare (ignorable out data))
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

;; Exif info
(defun get-ifds (jpeg-image)
  (when-let (app1 (find-if #'(lambda (seg) (typep seg 'exif-app1-segment))
			   (segments jpeg-image)))
    (let ((ifds '()))
      (do ((next-ifd (value (ifd app1)) (value (next-ifd (car ifds)))))
	  ((null next-ifd) (nreverse ifds))
	(push next-ifd ifds)))))

(defun maybe-pointer-value (mp)
  (if (typep (pointer mp) 'ifd-pointer-type)
      (value (pointer mp))
      (pointer mp)))

(defun get-ifd-fields (ifd)
  (labels ((get-ifd-fields (entries fields)
	     (if (null entries)
		 (nreverse fields)
		 (get-ifd-fields (cdr entries)
				 (cons (cons (id (car entries))
					     (maybe-pointer-value (car entries)))
				       fields)))))
    (get-ifd-fields (entries ifd) '())))

(defun get-tiff-fields (jpeg-image)
  (mapcan #'get-ifd-fields (get-ifds jpeg-image)))

(defun get-tiff-field (jpeg-image field)
  (cdr (assoc field (get-tiff-fields jpeg-image))))

(defun get-exif-fields (jpeg-image)
  (get-ifd-fields (get-tiff-field jpeg-image 'private-exif-ifd)))

(defun get-exif-field (jpeg-image field)
  (cdr (assoc field (get-exif-fields jpeg-image))))

(defun get-gps-fields (jpeg-image)
  (get-ifd-fields (get-tiff-field jpeg-image 'gps-info)))
  

