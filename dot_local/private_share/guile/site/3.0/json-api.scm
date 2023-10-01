(define-module (json-api)
  #:use-module (json)
  #:use-module (web client)
  #:export (json-post))

(define* (json-post uri
					#:key

                    (body #f)
                    (verify-certificate? #t)
                    (port (open-socket-for-uri uri
                                               #:verify-certificate?
                                               verify-certificate?))
                    (version '(1 . 1))
                    (keep-alive? #f)
                    (headers '())
                    (decode-body? #f)
                    (streaming? #f))
  (let* ((payload (if (null? body)
					  body
					  (scm->json-string body #:pretty #t)))
		 (length (unless (null? body)
				   (string-length payload)))
		 (headers (if (null? body)
					  headers
					  (cons `(content-length . ,length) headers))))
	(http-post uri
			   #:body payload
			   #:verify-certificate? verify-certificate?
			   #:port port
			   #:version version
			   #:keep-alive? keep-alive?
			   #:headers headers
			   #:decode-body? decode-body?
			   #:streaming? streaming?)))
