(in-package :acl)

(defun test-group ()
  (make-instance 'group
                 :name "test-group"
                 :query "SELECT ?s WHERE { ?s ?p ?o .}"
                 :parameters '("s")))

(defun test-group-description ()
  (make-instance 'group
                 :name "test-group"
                 :description "Description for test group"
                 :query "SELECT ?s WHERE { ?s ?p ?o .}"
                 :parameters '("s")))

(defun test-graph-spec ()
  (make-instance 'graph-spec
                 :name "test-graph-spec"
                 :graph "http://mu.semte.ch/graphs/public"))

(defun test-type-spec-no-predicate-spec ()
  (make-instance 'type-spec
                 :resource-type "persoon:Persoon"))

(defun test-type-spec-generic-predicate-spec ()
  (make-instance 'type-spec
                 :resource-type "persoon:Persoon"
                 :predicates `(,(make-instance 'predicate-spec :direction "->"))))

(defun test-type-spec ()
  (make-instance 'type-spec
                 :resource-type "persoon:Persoon"
                 :predicates (list (make-instance 'predicate-spec
                                                  :direction "->"
                                                  :predicate "foaf:firstName")
                                   (make-instance 'predicate-spec
                                                  :direction "->"
                                                  :predicate "foaf:familyName"))))

(defun test-predicate-spec-no-pred ()
  (make-instance 'predicate-spec :direction "->"))

(defun test-predicate-spec ()
  (make-instance 'predicate-spec :direction "->" :predicate "foaf:firstName"))

(defun test-graph-spec-with-simple-type-spec ()
  (make-instance 'graph-spec
                 :name "test-graph-spec"
                 :graph "http://mu.semte.ch/graphs/public"
                 :types `(,(test-type-spec-no-predicate-spec))))

(defun test-graph-spec-with-type-spec ()
  (make-instance 'graph-spec
                 :name "test-graph-spec"
                 :graph "http://mu.semte.ch/graphs/public"
                 :types (list (test-type-spec))))

(defun test-grant-read ()
  (make-instance 'grant
                 :right '(read)
                 :graph (test-graph-spec-with-type-spec)
                 :group (test-group)))

(defun test-grant-read-write ()
  (make-instance 'grant
                 :right '(read write)
                 :graph (test-graph-spec-with-type-spec)
                 :group (test-group)))

(defun test-configuration ()
  (make-instance 'configuration
                 :groups `(,(test-group))
                 :graphs `(,(test-graph-spec-with-type-spec))
                 :grants `(,(test-grant-read-write))))
