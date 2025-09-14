(in-package :odrl-parser)

(defparameter *backend*
  (if (find :docker *features*)
      ;; TODO: service name should be configurable
      ;; TODO: bypassing auth and going directly to virtuoso to simplify development
      "http://virtuoso:8890/sparql"
      "http://localhost:8896")
  "Backend triplestore to talk to.")

(defparameter output-directory
  (if (find :docker *features*)
      "config/"
      "examples/")
  "The directory generated files will be written to.")

(defparameter *port*
  (if (find :docker *features*) 80 8080)
  "The default port for the current setup.")

(defparameter *repository*
  (make-instance 'fuseki::virtuoso-repository
                 :name "main repository"
                 :server (make-instance 'fuseki::virtuoso-server
                                        :base-url *backend*)))

(defun boot ()
  "Function called when the service boots.

This function is called by the startup/load scripts provided as part of the `lisp-webservice' base
image."
  (format t "~& >> Welcome to odrl-parser!")
  (format t "~& >> Using backend: ~a" *backend*)
  (format t "~& >> Using port: ~a" *port*)
  (mu-support:boot)
  (format t "~& >> Finished boot function"))


;;
;; Service API
;;
;; NOTE (13/09/2025): Over time this should probably be re-implemented using some web-framework or
;; something.  But for the moment this suffices.

;; TODO(B): add argument to provide name for config file
(hunchentoot:define-easy-handler (load-policy :uri "/load-policy") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (load-policy-file)
  (format nil "~& >> Loaded policy from 'config.nt'"))

(hunchentoot:define-easy-handler (generate-config :uri "/generate-config") (policy-name)
  (setf (hunchentoot:content-type*) "text/plain")
  (generate-authorisation-configuration policy-name)
  (format nil "~& >> Generated authorisation configurations"))

(defun extract-filename (uri)
  "Extract a file name from the given uri."
  (car (last (cl-ppcre:split "/|:" uri))))

(defun generate-authorisation-configuration (&optional policy-name)
  "Generate an authorisation configuration for a policy with the given POLICY-NAME.

If POLICY-NAME is nil a configuration is generated for each `odrl:Set' resource found in the
backend.  Each policy is written to a file in `output-directory' with the policy's name used as
filename."
  (let ((policy-uris (if policy-name
                         `(,(format nil "ext:~a" policy-name))
                         (policy-retrieval:list-known-policies))))
    (loop
      for uri in policy-uris
      do (with-open-file
             (stream
              (concatenate 'string output-directory (extract-filename uri) ".lisp")
              :direction :output
              :if-exists :supersede)
           (write
            ;; TODO(A): handle errors/nil from `make-rule-set'
            (odrl:odrl-to-acl (policy-retrieval:make-rule-set uri))
            :stream stream)))))
