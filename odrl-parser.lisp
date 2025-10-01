(in-package :odrl-parser)

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
  (format t "~& >> Loaded policy from 'config.nt'"))

(hunchentoot:define-easy-handler (generate-config :uri "/generate-config") (policy-name)
  (setf (hunchentoot:content-type*) "text/plain")
  (generate-authorisation-configuration policy-name)
  (format t "~& >> Generated authorisation configurations"))

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
      do (handler-case
             (let ((conf (odrl:odrl-to-acl (policy-retrieval:parse-stored-policy uri))))
               (with-open-file
                 (stream
                  (concatenate 'string output-directory (extract-filename uri) ".lisp")
                  :direction :output
                  :if-exists :supersede)
               (write conf :stream stream)))
           (error (e)
             (format t "~& >> WARN: An error occurred when parsing policy \"~a\", no configuration written: \"~a\"" uri e))))))
