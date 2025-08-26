(in-package :odrl-parser)

(defparameter *backend*
  (if (find :docker *features*)
      ;; TODO: service name should be configurable
      "http://virtuoso:8890/sparql"
      "http://localhost:8896")
  "Backend triplestore to talk to.")

(defparameter *repository*
  (make-instance 'fuseki::virtuoso-repository
                 :name "main repository"
                 :server (make-instance 'fuseki::virtuoso-server
                                        :base-url *backend*)))

(defun ping-database ()
  (dex:get *backend*))

(defun select-type-query ()
  (fuseki:query *repository*
                (concatenate
                 'string
                 "SELECT DISTINCT ?type "
                 "WHERE { "
                 "  ?s a ?type . "
                 "}")))

(defun boot ()
  (format t "~& >> Welcome to odrl-parser!")
  (format t "~& >> Using backend: ~a" *backend*)
  (format t "~a" (select-type-query)))
