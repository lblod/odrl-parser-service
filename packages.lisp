(defpackage :sparql
  (:use :cl :mu-support)
  (:shadow :delete :insert :query)
  (:export #:*application-graph*
           #:*experimental-no-application-graph-for-sudo-select-queries*
           #:*query-log-types*
           #:with-update-group
           #:insert
           #:select
           #:delete
           #:delete-triples
           #:insert-triples
           #:update-triples
           #:query
           #:ask
           #:without-update-group
           #:update)
  (:documentation "Supports performing SPARQL queries against a backend."))

(defpackage :policy-retrieval
  (:use :cl :mu-support :alexandria)
  (:import-from :alexandria #:when-let*)
  (:export #:list-known-policies
           #:make-rule-set)
  (:local-nicknames (:mu :mu-support))
  (:documentation "Functionality to retrieve ODRL policies from a backend and convert them from plain triples to the service's ODRL model."))

(defpackage :acl
  (:use :cl)
  (:export #:configuration
           #:group
           #:graph-spec
           #:type-spec
           #:predicate-spec
           #:grant
           #:direction-string)
  (:documentation "Provides an implementation of sparql-parser's configuration DSL."))

(defpackage :shacl
  (:use :cl)
  (:export #:node-shape
           #:property-shape
           #:property-path
           #:shacl-to-acl)
  (:documentation "A simplified implementation of the Shapes Constraint Language (SHACL)."))

(defpackage :odrl
  (:use :cl)
  (:export #:rule-set
           #:party-collection
           #:asset-collection
           #:permission
           #:action
           #:odrl-to-acl)
  (:documentation "A simplified implementation of the ODRL information model."))

(defpackage :odrl-parser
  (:use :cl :mu-support :cl-ntriples)
  (:shadow :boot)
  (:export #:boot)
  (:documentation "Entrypoint for the service containing its API and configuration."))
