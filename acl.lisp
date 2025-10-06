(in-package #:acl)

;; Sparql-parser's configuration DSL
;;
;; This defines classes that roughly correspond to the concepts defined by sparql-parser's
;; configuration interface.

;; TODO(C): There typically is also some (fairly) standard boiler plate to configure the service instance (setting some variables to appropriate values, logging, etc.)
(defclass configuration ()
  ((prefixes :initarg :prefixes
             :initform nil
             :accessor prefixes)
   (groups :initarg :groups
           :initform '()
           :accessor groups)
   (graphs :initarg :graphs
           :accessor graphs
           :initform '())
   (grants :initarg :grants
           :accessor grants
           :initform '()))
  (:documentation "A configuration for the sparql-parser service consisting of a set of groups, graph specifications, and grants linking them."))

(defmethod initialize-instance :after ((configuration configuration) &key)
  (unless (slot-value configuration 'prefixes)
    (setf (slot-value configuration 'prefixes) (fuseki:get-prefix-alist))))

(defclass entity ()
  ((description :type (or null string) :initarg :description :initform nil))
  (:documentation "A common class that encompasses the different elements in sparql-parser's configuration DSL.  While entity descriptions are technically not part of sparql-parser's DSL, they are used to allow adding comments with additional information to the generated configurations."))

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

(defun prefix-uri (uri)
  "Return a prefixed version of URI if a suitable prefix is known, otherwise return URI.

The \"known\" prefixes are those returned by `fuseki:get-prefix-alist'."
  (let* ((prefixes (fuseki:get-prefix-alist))
         ;; NOTE (06/10/2025): Need to invert the order of arguments used in the `search' function
         ;; as we a looking for a cell whose cdr is a prefix of URI.
         (prefix (rassoc uri prefixes :test (lambda (u p) (search p u)))))
    (if prefix
        (let ((cl-ppcre:*allow-quoting* t))
          ;; NOTE (06/10/2025): We quote the regex to disable metacharacters as we want to replace
          ;; literal strings.
          (cl-ppcre:regex-replace
           (concatenate 'string "\\Q" (cdr prefix))
           uri
           ;; NOTE (06/10/2025): This is made into list with a single string to ensure the
           ;; replacement is inserted verbatim.
           `(,(concatenate 'string (car prefix) ":"))))
        uri)))

;;
;; Printing objects
;;
;; These methods convert objects to strings as they would be written in a sparql-parser
;; configuration.  Using `print-objects' has the advantage that this is automatically applied
;; everywhere, irrelevant of whether we are, for example, writing to a file or the printing error
;; messages.
(defmethod print-object ((object configuration) stream)
  (with-slots (prefixes graphs groups grants) object
    (format stream
            "~&(in-package :acl)~%~%~a~&~%;; Graphs~&~{~a~^~&~}~%;; Groups~&~{~a~^~&~}~%;; Grants~&~{~a~^~&~}"
            (format-prefixes prefixes)
            graphs
            groups
            grants)))

(defun format-prefix (prefix)
  "Format the cons cell PREFIX as plist property-value pair."
  (format nil ":~a \"~a\"" (car prefix) (cdr prefix)))

(defun format-prefixes (prefixes)
  "Format each element in PREFIXES as plist property-value pairs."
  (if (> (length prefixes) 0)
      (format nil
              "(define-prefixes~%~{~2t~a~^~&~})"
              (mapcar #'format-prefix prefixes))
      ""))

(defmethod print-object ((object group) stream)
  (with-slots (description name query parameters) object
    (format stream
            "~&~@[;; ~a~]~&(supply-allowed-group \"~a\"~@[~&~2t:parameters (~{\"~a\"~^ ~})~]~@[~&~2t:query \"~a\"~])~%~%"
            description
            name
            parameters
            query)))

(defmethod print-object ((object graph-spec) stream)
  (with-slots (description name graph types) object
    (format stream
            "~&~@[;; ~a~]~&(define-graph ~a (\"~a\")~@[~{~a~^~&~}~])~%~%"
            description
            name
            graph
            types)))

(defmethod print-object ((object type-spec) stream)
  (with-slots (resource-type predicates) object
    (format stream
            "~&~2t(\"~a\" ~:[-> _~;~:*~{~a~^~&~4t~}~])"
            (prefix-uri resource-type)
            predicates)))

(defmethod print-object ((object predicate-spec) stream)
  (with-slots (direction predicate) object
    (format stream "~a ~:[_~;~:*\"~a\"~]" direction (prefix-uri predicate))))

(defmethod print-object ((object grant) stream)
  (with-slots (right graph group) object
    (format stream
            "~&(grant (~{~a~^ ~})~&~2t:to-graph ~a~&~2t:for-allowed-group \"~a\")~%~%"
            right
            (name graph)
            (name group))))
