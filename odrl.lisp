(in-package :odrl)

;; ODRL information model
;;
;; An implementation of a simplified version of the ODRL information model.  This implementation is
;; intended to cover the parts of ODRL we currently need, and is not intended to support the entire
;; information model.  For example, this only supports Sets and Permissions, and no other types of
;; policies or rules.  Similarly, Constraints are not supported at all.
;;
;; Furthermore, this implementation explicitly deviates from ODRL's specification in some ways.
;; Consult the documentation of individual classes for more information.
(defclass concept ()
  ((uri :initarg :uri
        :reader uri))
  (:documentation "Base class for ODRL concepts."))

(defclass policy (concept)
  ((rules :initarg :rules
          :initform (error "At least one RULE must be supplied for a policy")
          :type list
          :reader rules)) ; odrl:permission
  (:documentation "An ODRL Policy consisting of a set of rules."))

(defclass rule-set (policy)
  ()
  (:documentation "An ODRL Set that represents any set of rules."))

(defclass party-collection (concept)
  ((name :initarg :name
         :initform (error "A NAME must be supplied for a party collection")
         :reader name) ; vcard:fn
   (description :initarg :description
                :initform nil
                :reader description) ; ext:description
   (parameters :initarg :parameters
               :initform nil
               :reader parameters) ; ext:queryParameters
   (query :initarg :query
          :initform nil
          :reader query)) ; ext:definedBy
  (:documentation "An ODRL party collection.  In contrast to the ODRL specification this does not explicitly contain member parties.  Instead members are essentially defined by the query, if the query returns a result the (implied) party is considered a member of the party collection."))

(defclass asset-collection (concept)
  ((name :initarg :name
         :type string
         :reader name
         :initform (error "A NAME must be supplied for an asset collection")) ; vcard:fn
   (description :initarg :description
                :initform nil
                :reader description) ; dct:description
   (graph :initarg :graph
          :initform (error "A GRAPH (prefix) must be supplied for an asset collection")
          :reader graph) ; ext:graphPrefix
   (assets :initarg :assets
           :type list ; of `shacl:node-shape's
           :reader assets)) ; ^odrl:partOf
  (:documentation "An ODRL Asset collection representing a graph.  In contrast to the ODRL specification this does explicitly refer to its contained assets, thereby modelling the inverse of the ODRL's partOf predicate.  This inversion simplifies converting ODRL policies to ACL configurations as it allows to iterate of the necessary assets when given an asset collection, which is in turn referenced by a rule for the starting point of the ODRL to ACL conversion.  Otherwise, one would somehow have to keep track of all asset instances and link them their collections.  A consequence of this is that the entity creating `asset-collection' instances is responsible for inverting the relations between assets and the asset collections they part of.  Furthermore, assets are represented as instances of `shacl:node-shape' and there is *no* explicit class for ODRL Assets."))

(defclass rule (concept)
  ((action :initarg :action
           :initform (error "An ACTION must be supplied for a rule")
           :type action
           :reader action) ; odrl:action
   (target :initarg :target
           :initform (error "A TARGET must be supplied for a rule")
           :type asset-collection
           :reader target) ; odrl:target
   (assignee :initarg :assignee
             :initform (error "An ASSIGNEE must be supplied for a rule")
             :type party-collection
             :reader assignee)) ; odrl:assignee
  (:documentation "An ODRL rule combines the common parts for permissions, prohibitions, and duties."))

(defclass permission (rule)
  ()
  (:documentation "An ODRL permission represents that an assignee is allowed to perform an action on a target."))

(defclass action (concept)
  ()
  (:documentation "An ODRL Action class which indicates an operation that can be performed on an asset.  The actual operation should be encoded in the URI of the action element.  Note that the conversion to ACL currently only supports two actions: `odrl:read' and `odrl:modify', specifying any other action will lead to errors."))


;;
;; Conversion to sparql-parser's ACL
;;
;; NOTE (12/09/2025): The need to pass the configuration under construction is, at best, messy and
;; would ideally be avoided.  Unfortunately, the need to keep track of some state is inherent to the
;; taken object-based approach and the need to ensure each object is converted exactly once.
;; Otherwise you risk ending up with configurations that contain, for example, the same graph
;; specification once for each grant specified for it.
(defgeneric odrl-to-acl (concept &optional configuration)
  (:documentation "Convert an ODRL concept to its corresponding sparql-parser configuration macro.  The `configuration' argument can be used to pass along the configuration being created.  This is necessary to reuse already created entities where necessary.  For example, when multiple permissions target the same asset collection, this collection should be converted to a graph specification exactly once and all generated grants should refer to the same instance."))

(defmethod odrl-to-acl ((concept policy) &optional (configuration (make-instance 'acl:configuration)))
  (mapcar (lambda (rule) (odrl-to-acl rule configuration)) (rules concept))
  configuration)

(defmethod odrl-to-acl ((concept rule-set) &optional configuration)
  (call-next-method))

;; TODO(A): Properly handle calls where `configuration' is nil, in that case one could just return a
;; new instance by default.
(defmethod odrl-to-acl ((concept party-collection) &optional configuration)
  (with-slots (description name query parameters) concept
    (acl:add-entity
     configuration
     (make-instance 'acl:group
                    :description description
                    :name name
                    :query query
                    :parameters parameters))))

(defmethod odrl-to-acl ((concept asset-collection) &optional configuration)
  (with-slots (description name graph assets) concept
    (acl:add-entity
     configuration
     (make-instance 'acl:graph-spec
                    :description description
                    :name name
                    :graph graph
                    :types (mapcar #'shacl:shacl-to-acl assets)))))

(defmethod odrl-to-acl ((concept permission) &optional configuration)
  (with-slots (action target assignee) concept
    (acl:add-entity
     configuration
     (make-instance 'acl:grant
                    :right `(,(odrl-to-acl action))
                    :graph (odrl-to-acl target configuration)
                    :group (odrl-to-acl assignee configuration)))))

(defmethod odrl-to-acl ((concept action) &optional configuration)
  (declare (ignore configuration))
  (with-slots (uri) concept
    (cond
      ((cl-ppcre:scan ".*read>?$" uri) "read")
      ((cl-ppcre:scan ".*modify>?$" uri) "write")
      (t (error "No matching right found for \"~a\"" uri)))))


;;
;; Varia
;;
(defmethod print-object ((object rule-set) stream)
  (print-unreadable-object (object stream)
    (with-slots (uri rules) object
      (format
       stream
       "<~a> odrl:permission ~{~2t~a~^~&~}"
       uri
       (mapcar #'uri rules)))))

(defmethod print-object ((object rule) stream)
  (print-unreadable-object (object stream)
    (format stream "~a" (uri object))))

(defmethod print-object ((concept action) stream)
  (print-unreadable-object (concept stream)
    (format stream "~a" (uri concept))))
