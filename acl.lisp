(in-package #:acl)

;; Sparql-parser's configuration DSL
;;
;; This defines classes that roughly correspond to the concepts defined by sparql-parser's
;; configuration interface.

;; TODO(B): A configuration typically also contains a `define-prefixes' entry, should be generated from graph specifications
;; TODO(C): There typically is also some (fairly) standard boiler plate to configure the service instance (setting some variables to appropriate values, logging, etc.)
(defclass configuration ()
  ((groups :initarg :groups
           :reader groups)
   (graphs :initarg :graphs
           :reader graphs)
   (grants :initarg :grants
           :reader grants))
  (:documentation "A configuration for the sparql-parser service consisting of a set of groups, graph specifications, and grants linking them."))

;; TODO(B): add constraint
(defclass group ()
  ((name :initarg :name
         :initform (error "A NAME must be supplied for a group")
         :reader name)
   (query :initarg :query
          :initform nil
          :reader query)
   (parameters :initarg :parameters
               :initform nil
               :reader parameters))
  (:documentation "A class containing the information for a `supply-allowed-groups' rule"))

(defclass graph-spec ()
  ((name :initarg :name
         :initform (error "A NAME must be supplied for a graph specification")
         :reader name)
   (graph :initarg :graph
          :initform (error "A GRAPH (prefix) must be supplied for a graph specification")
          :reader graph)
   (types :initarg :types
          :initform nil
          :type list ; of `type-spec's
          :reader types))
  (:documentation "A class for the `define-graph' macro"))

(defun to-kebab-case (str)
  "Convert a STR to kebab case.

Note, this is a simplified version that does *not* split camel case, any upper case letters will
simply be down cased."
  (string-downcase (cl-ppcre:regex-replace-all "\\s+|_" str "-")))

(defmethod initialize-instance :after ((group group) &key)
  (setf (slot-value group 'name) (to-kebab-case (name group))))

(defmethod initialize-instance :after ((graph graph-spec) &key)
  (setf (slot-value graph 'name) (to-kebab-case (name graph))))

(defclass type-spec ()
  ((resource-type :initarg :resource-type
                  :initform (error "A TYPE must be supplied for a type specification")
                  :reader resource-type)
   (predicates :initarg :predicates
               :initform nil
               :type list ; of `predicate-spec's
               :reader predicates))
  (:documentation "A class for a sparql-parser type specification"))

(defclass predicate-spec ()
  ((direction :initarg :direction
              :initform (error "A DIRECTION must be supplied for a type specification"))
   (predicate :initarg :predicate
              :initform nil
              :reader predicate))
  (:documentation "A predicate specification specifies in which direction to follow a given predicate"))

;; TODO(A): support scopes
(defclass grant (entity)
  ((right :initarg :right
          :initform (error "At least one RIGHT must be supplied for a grant")
          :accessor right
          :type list)
   (graph :initarg :graph
          :initform (error "A GRAPH must be supplied for a grant")
          :reader graph
          :type graph-spec)
   (group :initarg :group
          :initform (error "A GROUP must be supplied for a grant")
          :reader group
          :type group))
  (:documentation "Grant one or more rights to a group to access a graph."))
