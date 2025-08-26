(in-package :odrl-parser)

(defparameter *backend*
  (if (find :docker *features*)
      ;; TODO: service name should be configurable
      ;; TODO: bypassing auth and going directly to virtuoso to simplify development
      "http://virtuoso:8890/sparql"
      "http://localhost:8896")
  "Backend triplestore to talk to.")

(defparameter *port*
  (if (find :docker *features*) 80 8080)
  "The default port for the current setup.")

(defparameter *repository*
  (make-instance 'fuseki::virtuoso-repository
                 :name "main repository"
                 :server (make-instance 'fuseki::virtuoso-server
                                        :base-url *backend*)))

;; Server stuff
(defun boot ()
  "Function called when the service boots.

This function is called by the startup/load scripts provided as part of the `lisp-webservice' base
image."
  (format t "~& >> Welcome to odrl-parser!")
  (format t "~& >> Using backend: ~a" *backend*)
  (format t "~& >> Using port: ~a" *port*)
  (mu-support:boot)
  (format t "~& >> Finished boot function"))
