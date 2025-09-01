(in-package :odrl-parser)

(defparameter sparql:*application-graph*
  (s-url (or (uiop:getenv "ODRL_POLICY_GRAPH")
             "http://mu.semte.ch/graphs/odrl-policy"))
  "Default graph where ODRL policies should be located in.")

(defparameter sparql:*query-log-types* '(:default :update-group :update :query :ask)
  "If truthy, queries will be logged to *error-output*")

(defparameter sparql:*experimental-no-application-graph-for-sudo-select-queries* nil
  "when non-nil no application graph will be set when sending out sudo
  queries, thus resulting in queries across the full database.")
