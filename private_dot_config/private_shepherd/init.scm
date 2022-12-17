;; init.scm -- Shepherd Configuration
(use-modules (shepherd service)
             (oop goops)
             (ice-9 rdelim))

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

  (define (get-namespace ns)
    (when ns
        (string-append "--namespace=" ns)))

  (define (get-service-namespace-from-pid pid)
      (let* ((proc-cmdline    (string-append "/proc/" (number->string pid) "/cmdline"))
             (cmdline         (string-split (call-with-input-file proc-cmdline read-line) #\x00))
             (namespace-flag  (caddr cmdline))
             (namespace       (cadr (string-split namespace-flag #\=))))
        namespace))

  (define (get-staging-environment-from-namespace ns)
    (let ((parts (string-split ns #\-)))
      (cond ((and (= 3 (length parts))
                  (string=? "core" (caddr parts)))   ;; vX Environments
             (car parts))
            ((and (= 2 (length parts))
                  (string=? "io" (car parts)))       ;; Ephemeral Environments
             (cadr parts))
            (else "Port forwarding is outside a staging environment."))))

  (define (action-namespace pid . _)
    (when pid
      (display (get-service-namespace-from-pid pid))
      (newline)))

  (define (action-environment pid . _)
    (when pid
      (let* ((ns  (get-service-namespace-from-pid pid))
             (env (get-staging-environment-from-namespace ns)))
        (display env)
        (newline))))

  (let* ((service   (get-service))
         (log-file  (get-logfile))
         (provide   (list (string->symbol (string-append "proxy-" name))
                          (string->symbol name)))
         (docstring (string-join (list "Kubernetes proxy for" name "service"))))
    (make <service>
      #:docstring docstring
      #:provides  provide
      #:start     (lambda args
                    (let* ((namespace (if (null? args)
                                          (get-namespace namespace)
                                          (get-namespace (car args))))
                           (command (oc-command "port-forward" namespace service port)))
                      (apply (make-forkexec-constructor command #:log-file log-file) args)))
      #:stop      (make-kill-destructor)
      #:actions   (make-actions
                   (namespace
                    "Display the current namespace"
                    action-namespace)
                   (environment
                    "Display the current staging environment"
                    action-environment)
                   (logs
                    "Display the path for Logfile"
                    (lambda args (display log-file)))))))

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
                   (make-oc-proxy "coreapi"
                                  #:port "30001:80"
                                  #:log-file "coreapi"
                                  #:namespace "qa-rapyuta-core"
                                  #:service "apiserver")
                   (make-oc-proxy "rip"
                                  #:port "30002:80"
                                  #:log-file "rip"
                                  #:namespace "qa-rapyuta-core"
                                  #:service "rip")
                   (make-oc-proxy "authz"
                                  #:port "30003:80"
                                  #:log-file "authz"
                                  #:namespace "qa-rapyuta-core"
                                  #:service "authz")
                   (make-oc-proxy "iobroker"
                                  #:port "30004:80"
                                  #:log-file "iobroker"
                                  #:namespace "qa-rapyuta-core"
                                  #:service "iobroker")
                   (make-oc-proxy "helmbroker"
                                  #:port "30005:80"
                                  #:log-file "helmbroker"
                                  #:namespace "qa-rapyuta-core"
                                  #:service "helmbroker")
                   (make-oc-proxy "devicebroker"
                                  #:port "30006:80"
                                  #:log-file "devicebroker"
                                  #:namespace "qa-rapyuta-core"
                                  #:service "devicebroker")
                   (make-oc-proxy "es"
                                  #:port "39001:9200"
                                  #:log-file "test-es"
                                  #:namespace "test-es"
                                  #:service "elasticsearch-master")
                   (make-io-service "apiserver"
                                    #:v2? #t
                                    #:args '("--config" "config.yaml")))
