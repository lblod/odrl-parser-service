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

(defpackage :odrl-parser
  (:use :cl :mu-support)
  (:shadow :boot)
  (:export #:boot)
  (:documentation "Entrypoint for the service containing its API and configuration."))
