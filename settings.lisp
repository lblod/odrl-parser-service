(in-package :odrl-parser)

;; Sparql
(defparameter sparql:*application-graph*
  (s-url (or (uiop:getenv "ODRL_POLICY_GRAPH")
             "http://mu.semte.ch/graphs/odrl-policy"))
  "Default graph where ODRL policies should be located in.")

(defparameter sparql:*query-log-types* '(:default :update-group :update :query :ask)
  "If truthy, queries will be logged to *error-output*")

(defparameter sparql:*experimental-no-application-graph-for-sudo-select-queries* nil
  "when non-nil no application graph will be set when sending out sudo
  queries, thus resulting in queries across the full database.")

;; Backend
(defparameter *port*
  (if (find :docker *features*) 80 8080)
  "The default port for the current setup.")

(defparameter *backend*
  (if (find :docker *features*)
      ;; TODO: service name should be configurable
      ;; TODO: bypassing authz and going directly to virtuoso to simplify development
      "http://virtuoso:8890/sparql"
      "http://localhost:8896")
  "Backend triplestore to talk to.")

(defparameter *repository*
  (make-instance 'fuseki::virtuoso-repository
                 :name "main repository"
                 :server (make-instance 'fuseki::virtuoso-server
                                        :base-url *backend*)))

;; TODO: Might make this configurable
(defparameter output-directory
  (if (find :docker *features*)
      "config/"
      "examples/")
  "The directory generated files will be written to.")
