(in-package :shacl)

(defun test-property-shape-no-path ()
  (make-instance 'property-shape))

(defun test-property-shape-simple-path ()
  (make-instance 'property-shape :path "skos:inScheme"))

(defun test-property-shape-property-path ()
  (make-instance 'property-shape
                 :path (make-instance 'property-path
                                      :predicate-path "sh:inversePath"
                                      :object "regorg:orgStatus")))

(defun test-property-shape-property-path-all ()
  (make-instance 'property-shape
                 :path (make-instance 'property-path
                                      :predicate-path "sh:inversePath"
                                      :object "ext:all")))

(defun test-node-shape-simple-target ()
  (make-instance 'node-shape :target-class "mandaat:Mandataris"))

(defun test-node-shape-with-simple-property ()
  (make-instance 'node-shape
                 :target-class "mandaat:Mandataris"
                 :properties `(,(test-property-shape-simple-path))))

(defun test-node-shape-with-property-path ()
  (make-instance 'node-shape
                 :target-class "mandaat:Mandataris"
                 :properties `(,(test-property-shape-property-path))))

(defun test-node-shape-with-multiple-properties ()
  (make-instance 'node-shape
                 :target-class "mandaat:Mandataris"
                 :properties `(,(test-property-shape-property-path)
                               ,(test-property-shape-simple-path)
                               ,(test-property-shape-property-path-all))))

(defun test-node-shape-with-multiple-properties-not ()
  (make-instance 'node-shape
                 :target-class "mandaat:Mandataris"
                 :notp t
                 :properties `(,(test-property-shape-property-path)
                               ,(test-property-shape-simple-path)
                               ,(test-property-shape-property-path-all))))
