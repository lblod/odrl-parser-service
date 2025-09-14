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
   ;; NOTE (04/09/2025): The following is a list of `shacl:node-shape's that are contained in the
   ;; node shape linked to via `ext:shaclShape'.  The overarching node shape used in the raw policy
   ;; data is not explicitly retained.  This simplifies the conversion to ACL as it allows to simply
   ;; iterate over node shapes without having to consider multiple layers of node shapes.
   (shapes :initarg :shapes
           :initform (error "At least one element for SHAPES must be supplied for an asset collection")))
  (:documentation "An ODRL Asset collection representing a graph.  In contrast to the ODRL specification this does not explicitly contain assets.  Instead this uses SHACL shapes to define exactly which assets are part of the collection.  Essentially each node shape in the `shapes' list could be considered an asset."))

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
;; Varia
;;
(defmethod print-object ((concept action) stream)
  (print-unreadable-object (concept stream)
    (format stream "~a" (uri concept))))
