(in-package #:acl)

;; Sparql-parser's configuration DSL
;;
;; This defines classes that roughly correspond to the concepts defined by sparql-parser's
;; configuration interface.

;; TODO(B): A configuration typically also contains a `define-prefixes' entry, should be generated from graph specifications
;; TODO(C): There typically is also some (fairly) standard boiler plate to configure the service instance (setting some variables to appropriate values, logging, etc.)
(defclass configuration ()
  ((groups :initarg :groups
           :initform '()
           :accessor groups)
   (graphs :initarg :graphs
           :accessor graphs
           :initform '())
   (grants :initarg :grants
           :accessor grants
           :initform '()))
  (:documentation "A configuration for the sparql-parser service consisting of a set of groups, graph specifications, and grants linking them."))

(defclass entity ()
  ()
  (:documentation "A common class class to capture the different configuration entities."))

(defclass named-entity (entity)
  ((name :initarg :name
         :initform (error "A NAME must be supplied for a named entity")
         :reader name))
  (:documentation "An common class to capture configuration entities that have a name."))

;; TODO(B): add constraint
(defclass group (named-entity)
  ((query :initarg :query
          :initform nil
          :reader query)
   (parameters :initarg :parameters
               :initform nil
               :reader parameters))
  (:documentation "A class containing the information for a `supply-allowed-groups' rule"))

(defclass graph-spec (named-entity)
  ((graph :initarg :graph
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

(defun direction-string (inversep notp)
  "Determine the correct direction symbol for a predicate specification."
  (cond
    ((and inversep notp) "<x")
    ((and inversep (not notp)) "<-")
    ((and (not inversep) notp) "x>")
    (t "->")))

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


;;
;; Constructing configurations
;;
(defgeneric matching-entity (configuration entity)
  (:documentation "Return the entity instance in the configuration that \"matches\" the given one.  The exact meaning of \"matching\" depends on the type of the entity."))

;; NOTE (13/09/2025): The following methods essentially define what it means for two `entity' to be
;; equal.  For `group's and `graph's equality means having the same value for their `name' slot.
;; For `grant's equality means referencing a `group' *and* `graph' with the same name.  In a more
;; mature implementation these equality definitions would be incorporated in the object system.
;; The underlying reasoning is that sparql-parser, presumably, requires unique names to correctly identify entities.
(defmethod matching-entity ((configuration configuration) (entity group))
  (find-if
   (lambda (group) (string= (name entity) (name group)))
   (groups configuration)))

(defmethod matching-entity ((configuration configuration) (entity graph-spec))
  (find-if
   (lambda (graph) (string= (name entity) (name graph)))
   (graphs configuration)))

(defmethod matching-entity ((configuration configuration) (entity grant))
  (find-if
   (lambda (grant)
     (and (string= (name (group grant)) (name (group entity)))
          (string= (name (graph grant)) (name (graph entity)))))
   (grants configuration)))


(defgeneric add-entity (configuration entity)
  (:documentation "Add an ENTITY to a CONFIGURATION.  The function will return the `entity' instance that is actually added to the CONFIGURATION.  This returned `entity' is either the provided ENTITY itself, or a `matching-entity' that was already part of the CONFIGURATION."))

(defmethod add-entity ((configuration configuration) (named-entity graph-spec))
  (or (matching-entity configuration named-entity)
      (progn
        (push named-entity (graphs configuration))
        named-entity)))

(defmethod add-entity ((configuration configuration) (named-entity group))
  (or (matching-entity configuration named-entity)
      (progn
        (push named-entity (groups configuration))
        named-entity)))

(defmethod add-entity ((configuration configuration) (entity grant))
  (let ((matching-grant (matching-entity configuration entity)))
    (if matching-grant
        (progn
          (setf (right matching-grant)
                (union (right entity) (right matching-grant)))
          matching-grant)
        (progn
          (push entity (grants configuration))
          entity))))

;;
;; Printing objects
;;
;; These methods convert objects to strings as they would be written in a sparql-parser
;; configuration.  Using `print-objects' has the advantage that this is automatically applied
;; everywhere, irrelevant of whether we are, for example, writing to a file or the printing error
;; messages.
(defmethod print-object ((object configuration) stream)
  (with-slots (graphs groups grants) object
    (format stream
            ";; Graphs~&~{~a~^~&~}~%;; Groups~&~{~a~^~&~}~%;; Grants~&~{~a~^~&~}"
            graphs
            groups
            grants)))

(defmethod print-object ((object group) stream)
  (with-slots (name query parameters) object
    (format stream
            "~&(supply-allowed-group \"~a\"~@[~&~2t:parameters (~{\"~a\"~^ ~})~]~@[~&~2t:query \"~a\"~])~%~%"
            name
            parameters
            query)))

(defmethod print-object ((object graph-spec) stream)
  (with-slots (name graph types) object
    (format stream
            "~&(define-graph ~a (\"~a\")~@[~{~a~^~&~}~])~%~%"
            name
            graph
            types)))

(defmethod print-object ((object type-spec) stream)
  (with-slots (resource-type predicates) object
    (format stream
            "~&~2t(\"~a\" ~:[-> _~;~:*~{~a~^~&~4t~}~])"
            resource-type
            predicates)))

(defmethod print-object ((object predicate-spec) stream)
  (with-slots (direction predicate) object
    (format stream "~a ~:[_~;~:*\"~a\"~]" direction predicate)))

(defmethod print-object ((object grant) stream)
  (with-slots (right graph group) object
    (format stream
            "~&(grant (~{~a~^ ~})~&~2t:to-graph ~a~&~2t:for-allowed-group \"~a\")~%~%"
            right
            (name graph)
            (name group))))
