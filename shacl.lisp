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
