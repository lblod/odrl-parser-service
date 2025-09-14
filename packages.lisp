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

(defpackage :acl
  (:use :cl)
  (:export #:configuration
           #:group
           #:graph-spec
           #:type-spec
           #:predicate-spec
           #:grant)
  (:documentation "Provides an implementation of sparql-parser's configuration DSL."))

(defpackage :shacl
  (:use :cl)
  (:export #:node-shape
           #:property-shape
           #:property-path)
  (:documentation "A simplified implementation of the Shapes Constraint Language (SHACL)."))

(defpackage :odrl-parser
  (:use :cl :mu-support :cl-ntriples)
  (:shadow :boot)
  (:export #:boot)
  (:documentation "Entrypoint for the service containing its API and configuration."))
