(in-package :odrl-parser)

;; TODO: change to configuration-graph or something
(defparameter sparql:*application-graph*
  (s-url (or (uiop:getenv "MU_APPLICATION_GRAPH")
             "http://mu.semte.ch/application"))
  "Standard graph for all sparql queries.")

(defparameter sparql:*query-log-types* '(:default :update-group :update :query :ask)
  "If truthy, queries will be logged to *error-output*")

(defparameter sparql:*experimental-no-application-graph-for-sudo-select-queries* nil
  "when non-nil no application graph will be set when sending out sudo
  queries, thus resulting in queries across the full database.")
