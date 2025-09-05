(in-package #:shacl)

;; Shapes Constraint Language (SHACL)
;;
;; A, very, simplified implementation of SHACL.  This implementation is strictly limited to the
;; elements of SHACL we need in order to express which triples should be considered part of some
;; asset collection.
(defclass shape ()
  ((uri :initarg :uri)
   (target-class :initarg :target-class
                 :initform nil
                 :reader target-class)) ; sh:targetClass
  (:documentation "A SHACL shape."))

(defclass node-shape (shape)
  ((properties :initarg :properties
               :initform nil
               :reader properties) ; sh:property*
   ;; NOTE (04/09/2025): Used to indicate whether the property shapes are surrounded by a
   ;; `sh:not'. This is a simplification, ideally we can capture and process constraints
   ;; (components) in general.
   (notp :initarg :notp
         :type boolean
         :initform nil))
  (:documentation "A SHACL node shape"))

(defclass property-shape (shape)
  ((path :initarg :path
         :initform nil)) ; value is a predicate URI or a `property-path' instance
  (:documentation "A SHACL property shape"))

(defclass property-path ()
  ((predicate-path :initarg :predicate-path
                   :initform (error "A PREDICATE-PATH must be supplied for a property path"))
   (object :initarg :object
           :initform (error "An OBJECT must be supplied for a property path")
           :reader object))
  (:documentation "A SHACL property path."))


;;
;; Conversion to sparql-parser's ACL
;;
(defgeneric shacl-to-acl (shape &optional notp)
  (:documentation "Convert a SHACL shape to its corresponding sparql-parser entity."))

(defmethod shacl-to-acl ((shape node-shape) &optional notp)
  (with-slots (target-class properties notp) shape
    (make-instance 'acl:type-spec
                   :resource-type target-class
                   :predicates (mapcar (lambda (prop) (shacl-to-acl prop notp)) properties))))

(defun is-empty-node-p (path)
  "Check whether PATH is the special uri for an empty node.

The special uri was introduced to allow user to specify \"all predicates\" in a policy, as one would use `_' in a sparql-parser configuration.  This special uri was needed because in SHACL property paths must have a value for their object and otherwise we could not express sparql-parser rules of the of the form `<- _' or `<x _'. "
  (member path '("ext:all" "http://mu.semte.ch/vocabularies/ext/all") :test #'string=))

(defmethod shacl-to-acl ((shape property-shape) &optional notp)
  ;; If value of `path' is
  ;; - a URI: (make-... :direction "->" :predicate path)
  ;; - a `property-path':
  ;;   + parse its `predicate-path' to determine value for :direction
  ;;   + use its `object' as value for :predicate
  (with-slots (path) shape
    (make-instance
     'acl:predicate-spec
     ;; NOTE (13/09/2025): The simplification of using the mere existence of a property path to mean
     ;; invert the direction depends on the fact that we use no other property paths than
     ;; `sh:inversePath'.  This should be generalised to actually check which `predicate-path' is
     ;; used.
     :direction (acl:direction-string (typep path 'property-path) notp)
     :predicate (if (typep path 'property-path)
                    (unless (is-empty-node-p (object path)) (object path))
                    (unless (is-empty-node-p path) path)))))
