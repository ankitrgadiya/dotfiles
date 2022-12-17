;; init.scm -- Shepherd Configuration
(use-modules (shepherd service)
             (oop goops)
             (ice-9 popen))


;; Postgres - An open-source object-relational database
(define (make-postgres)
  (define (control-command . args)
    (string-join
     (cons* "pg_ctl"
            (string-append "--pgdata=" (getenv "HOME") "/.local/var/databases/postgres")
            (string-append "--log="    (getenv "HOME") "/.local/var/log/postgres/postgres.log")
            args)))
  (define (client-command . args)
    (string-join
     (cons* "psql"
            (string-append "--host=" "/run/user/" (number->string (getuid)) "/postgresql")
            "--dbname=postgres"
            args)))
  (make <service>
    #:docstring "An open-source object-relational database"
    #:provides  '(postgres sql-db)
    #:start     (make-system-constructor (control-command "start"))
    #:stop      (make-system-constructor (control-command "stop"))
    #:actions   (make-actions
                 (reload
                  "Re-load the configuration"
                  (make-system-constructor (control-command "reload"))))))

;; Caddy - A modern webserver written in Go.
(define (make-caddy)
  (define (get-base-directory)
    (string-append (getenv "HOME")
                   "/.config/caddy"))

  (define (get-log-file)
    (string-append (getenv "HOME")
                   "/.local/var/log/caddy/caddy.log"))

  (define (get-config-file)
    (string-append (get-base-directory) "/Caddyfile"))

  (define (caddy-command . args)
    (cons* "caddy" args))

  (let* ((base-directory    (get-base-directory))
         (config-file       (get-config-file))
         (log-file          (get-log-file))
         (run-command       (caddy-command "run" "--environ" "--config"
                                           config-file))
         (validate-command  (caddy-command "validate" "--config"
                                           config-file))
         (reload-command    (caddy-command "reload" "--config"
                                           config-file)))
    (make <service>
      #:docstring "A modern webserver written in Go."
      #:provides  (list 'caddy 'webserver 'reverse-proxy)
      #:start     (make-forkexec-constructor run-command
                                             #:directory base-directory
                                             #:log-file  log-file)
      #:stop      (make-kill-destructor)
      #:actions   (make-actions
                   (validate
                    "Validate the Caddyfile"
                    (make-system-constructor (string-join validate-command)))
                   (reload
                    "Reload the Caddyfile"
                    (make-system-constructor (string-join reload-command)))))))

;; OC Proxy - A function to generate Kubernetes Proxy Services.
(define* (make-oc-proxy name
                        #:key
                        (port      "80")
                        (service    #f)
                        (log-file   #f)
                        (namespace  #f))

  (define (oc-command . args)
    (cons* (string-append (getenv "HOME") "/.local/bin/oc")
           (filter (negate unspecified?) args)))

  (define (get-service)
    (string-append "svc/" (if service service name)))

  (define (get-logfile)
    (string-append (getenv "HOME")
                   "/.local/var/log/oc/"
                   (if log-file log-file name)))

  (define (get-namespace)
    (when namespace
        (string-append "--namespace=" namespace)))

  (let* ((service   (get-service))
         (namespace (get-namespace))
         (log-file  (get-logfile))
         (command   (oc-command "port-forward" namespace service port))
         (provide   (list (string->symbol (string-append "proxy-" name))
                          (string->symbol name)))
         (docstring (string-join (list "Kubernetes proxy for" name "service"))))

    (make <service>
      #:docstring docstring
      #:provides  provide
      #:start     (make-forkexec-constructor command #:log-file log-file)
      #:stop      (make-kill-destructor)
      #:actions   (make-actions
                   (logs
                    "Display the path for Logfile"
                    (lambda args (display log-file)))
                   (command
                    "Displays the command used"
                    (lambda args (display (string-join command))))))))

;; IO Service - A function to generate Rapyuta IO Services.
(define* (make-io-service name
                          #:key
                          (args     '())
                          (v2?      #f)
                          (log-file #f))

  (define (get-base-directory)
    (string-append (getenv "HOME")
                   "/.local/src/rapyuta_io"
                   (if v2?
                       (string-append "/v2")
                       (string-append "/"))))

  (define (get-go-files)
    (if v2?
        (string-append "./cmd/" name "/")
        (string-append "./pkg/" name "/cmd/")))

  (define (go-command . args)
    (cons* "go" (filter (negate unspecified?) args)))

  (define (get-logfile)
    (string-append (get-base-directory)
                   "/ankit/logs/"
                   (if log-file log-file name)))

  (define (get-provides)
    (list (if v2?
              (string->symbol (string-append "v2-" name))
              name)))

  (let* ((base-directory (get-base-directory))
         (go-files       (get-go-files))
         (command        (apply go-command (cons* "run" go-files args)))
         (provides       (get-provides))
         (docstring      (string-join (list "Rapyuta IO service" name)))
         (log-file       (get-logfile)))
    (make <service>
      #:docstring docstring
      #:provides  provides
      #:requires  (list 'sql-db)
      #:start     (make-forkexec-constructor command
                                             #:log-file  log-file
                                             #:directory base-directory)
      #:stop      (make-kill-destructor)
      #:actions   (make-actions
                   (logs
                    "Display the path for Logfile"
                    (lambda args (display log-file)))
                   (command
                    "Displays the command used"
                    (lambda args (display (string-join command))))))))

(register-services (make-postgres)
                   (make-caddy)
                   (make-oc-proxy "authz"
                                  #:port "8081:80"
                                  #:log-file "authz"
                                  #:service "authz")
                   (make-oc-proxy "es"
                                  #:port "9200:9200"
                                  #:log-file "test-es"
                                  #:namespace "test-es"
                                  #:service "elasticsearch-master")
                   (make-io-service "apiserver"
                                    #:v2? #t
                                    #:args '("--config" "config.yaml")))
