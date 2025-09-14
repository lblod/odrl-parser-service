(in-package :odrl)

(defun test-party ()
  (make-instance 'party-collection :name "test-party"))

(defun test-party-with-description ()
  (make-instance 'party-collection
                 :name "test-party-with-description"
                 :description "Description of test party collection."))

(defun test-party-with-param ()
  (make-instance 'party-collection
                 :name "test-party-param"
                 :description "Description of test party collection with parameters"
                 :parameters '("param-one" "param-two")))

(defun test-party-with-query ()
  (make-instance 'party-collection
                 :name "test-party-with-query"
                 :description "Description of test party collection with a query"
                 :query "SELECT blah blah bah"))

(defun test-party-with-query-and-param ()
  (make-instance 'party-collection
                 :name "test-party-query-param"
                 :description "Description of test party collection with a query and parameters"
                 :parameters '("param-one" "param-two")
                 :query "SELECT ?param-one ?param-two
                         WHERE {
                           blah blah
                         }"))

(defun test-asset ()
  (make-instance 'asset-collection
                 :name "some-asset-collection"
                 :description "Description for some asset collection"
                 :graph "http://mu.semte.ch/graphs/public"
                 :shapes `(,(shacl::test-node-shape-simple-target)
                           ,(shacl::test-node-shape-with-multiple-properties))))

(defun test-action-read ()
  (make-instance 'action :uri "odrl:read"))

(defun test-action-modify ()
  (make-instance 'action :uri "odrl:modify"))

(defun test-permission-read ()
  (make-instance 'permission
                 :action (test-action-read)
                 :target (test-asset)
                 :assignee (test-party-with-query-and-param)))

(defun test-permission-modify ()
  (make-instance 'permission
                 :action (test-action-modify)
                 :target (test-asset)
                 :assignee (test-party-with-query-and-param)))

(defun test-policy ()
  (make-instance 'rule-set
                 :rules `(,(test-permission-read) ,(test-permission-modify))))
