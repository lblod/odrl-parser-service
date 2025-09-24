(in-package :policy-retrieval)

;; Functionality to read ODRL policies form the backend an convert them to the service's internal
;; ODRL model.
;;
;; NOTE (13/09/2025): This is currently in a very rough state and requires at least to following
;; improvements:
;; - Improve the overall quality and robustness of the code.
;; - Reduce the number of queries performed, for example by retrieving most data in a single
;;   construct query.  Because the rdf:list in the node shape can have an arbitrary length this will
;;   probably need to be retrieved separately.
;; - Prevent the creation of duplicate objects. Currently, the ODRL conversion starts from the
;;   contained rules, as these resources are linked from a policy resource.  For each party and
;;   asset collection linked to a rule a *new* object will be created, irrelevant of whether that
;;   collection was already processed before.  This requires either changing how the data is
;;   retrieved and parsed (related to the previous point) or keeping some state to check whether a
;;   suitable object was already created (similar to what is done in the odrl-to-acl conversion.)
;; - Extract and generalise the parsing of the jsown objects received as input.  Currently this is
;;   rather ad-hoc and fragile.
;; - Convert the full URIs in replied to prefixed ones where possible.  This will make any
;;   configuration that is eventually generated from the data easier to read.

;; Add the used prefixes
(add-prefix "ext" "http://mu.semte.ch/vocabularies/ext/")
(add-prefix "odrl" "http://www.w3.org/ns/odrl/2/")
(add-prefix "dct" "http://purl.org/dc/terms/")
(add-prefix "vcard" "http://www.w3.org/2006/vcard/ns#")
(add-prefix "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(add-prefix "mandaat" "http://data.vlaanderen.be/ns/mandaat#")
(add-prefix "skos" "http://www.w3.org/2004/02/skos/core#")
(add-prefix "sh" "http://www.w3.org/ns/shacl#")
(add-prefix "lmb" "http://lblod.data.gift/vocabularies/lmb/")
(add-prefix "besluit" "http://data.vlaanderen.be/ns/besluit#")
(add-prefix "regorg" "http://www.w3.org/ns/regorg#")
(add-prefix "persoon" "http://data.vlaanderen.be/ns/persoon#")

(defparameter *policy-type* "odrl:Set"
  "The resource type used for ODRL policies.")

(defun query-for-policies ()
  "Retrieve a list of the URIs of all ODRL policies."
  (sparql:select
   (format nil "DISTINCT ~a" (mu:s-var "policy"))
   (format nil "~a a ~a ." (s-var "policy") *policy-type*)))

(defun list-known-policies ()
  "Construct a list of the resource URIs all ODRL policies found in the backend."
  (mapcar (lambda (policy) (jsown:val (jsown:val policy "policy") "value")) (query-for-policies)))

(defparameter subject-var (s-var "subject"))

(defparameter predicate-var (s-var "predicate"))

(defparameter object-var (s-var "object"))

;; TODO(C): use proper regex for URIs
(defun urip (str)
  "Check whether a string is a URI."
  (cl-ppcre:scan "https?://.+" str))

(defun expand-uri (uri)
  "Expand the URI if it is prefixed, other just return the provided URI."
  (if (urip uri)
      uri
      (when-let* ((split-uri (cl-ppcre:split ":" uri))
                  (prefix-exp (cl-fuseki:get-prefix (car split-uri))))
        (format nil "~a~a" prefix-exp (cadr split-uri)))))

(defun escape-uri (uri)
  "Escape the given URI."
  (sparql-escape (s-url uri)))

;; TODO(C): Be more flexible/robust with the URI argument.
;; TODO(B): Change to construct query
(defun retrieve-triples (uri)
  "Retrieve the triples for a resource with the given URI.

The provided URI can be a string containing a full resource uri or a uri prefixed with a known
prefix."
  (let ((uri (if (urip uri) (escape-uri uri) uri)))
    (sparql:select
     (format nil "DISTINCT ~a ~a ~a" subject-var predicate-var object-var)
     (format nil "~a ~a ~a .~&FILTER (~a = ~a)" subject-var predicate-var object-var subject-var uri))))


;;
;; Parse jsown objects
;;
;; TODO(C): can probably be simplified using `jsown:filter'
(defun object-from-triple (triple)
  "Return the object for a TRIPLE."
  (jsown:val triple (raw-content object-var)))

(defun object-value-from-triple (triple)
  "Return the value of the object for TRIPLE."
  (term-value-from-triple (raw-content object-var) triple))

(defun predicate-value-from-triple (triple)
  "Return the value of predicate for TRIPLE."
  (term-value-from-triple (raw-content predicate-var) triple))

(defun term-value-from-triple (term triple)
  "Return the value of TERM in TRIPLE."
  (jsown:val (jsown:val triple term) "value"))

;; TODO(C): can probably be simplified using `jsown:filter'
;; TODO(C): be more flexible/robust with the PREDICATE argument
(defun triple-for-predicate-p (triple predicate)
  "Check whether a TRIPLE has the given value as its PREDICATE.

The TRIPLE should be an object usable by the `jsown' library.
The PREDICATE should be a string containing a prefixed predicate of its full uri, excluding the
surrounding \"<\" and \">\"."
  (string=
   (jsown:val (jsown:val triple (raw-content predicate-var)) "value")
   (expand-uri predicate)))

(defun filter-triples-for-predicate (predicate triples)
  "Retrieve the triples for a given PREDICATE in a set of TRIPLES.

The PREDICATE should be a string containing a prefixed predicate of its full uri, excluding the
surrounding \"<\" and \">\"."
  (remove-if-not (lambda (triple) (triple-for-predicate-p triple predicate)) triples))

(defun find-triple-for-predicate (predicate triples)
  "Retrieve the (first) triple for a given PREDICATE in a set of TRIPLES.

The PREDICATE should be a string containing a prefixed predicate of its full uri, excluding the
surrounding \"<\" and \">\"."
  (find-if (lambda (triple) (triple-for-predicate-p triple predicate)) triples))

(defun object-value-for-predicate (predicate triples)
  "Retrieve the value of the (first) object for PREDICATE in TRIPLES."
  (object-value-from-triple (find-triple-for-predicate predicate triples)))

(defun collect-object-for-predicate (predicate triples)
  "Collect a list of all object values for PREDICATE in triples."
  (mapcar #'object-value-from-triple (filter-triples-for-predicate predicate triples)))


;;
;; Conversion to ODRL
;;
(defun make-rule-set (uri)
  "Make a `rule-set' instance for the data linked to the resource with URI."
  (when-let* ((triples (retrieve-triples uri))
              (permissions (filter-triples-for-predicate "odrl:permission" triples)))
    (make-instance 'odrl:rule-set
                   :uri uri
                   :rules (mapcar #'make-permission permissions))))

(defun make-permission (triple)
  "Make a `permission' instance for the permission resource linked in TRIPLE."
  (when-let* ((uri (object-value-from-triple triple))
              (triples (retrieve-triples uri))
              (action (find-triple-for-predicate "odrl:action" triples))
              (target (find-triple-for-predicate "odrl:target" triples))
              (assignee (find-triple-for-predicate "odrl:assignee" triples)))
    (make-instance 'odrl:permission
                   :uri uri
                   :action (make-action action)
                   :target (make-asset-collection target)
                   :assignee (make-party-collection assignee))))

(defun make-action (triple)
  "Make an `action' instance with the given TRIPLE."
  (make-instance 'odrl:action :uri (object-value-from-triple triple)))

(defun make-party-collection (triple)
  "Make an `party-collection' instance for the assignee linked in TRIPLE."
  (when-let* ((uri (object-value-from-triple triple))
              (triples (retrieve-triples uri))
              (name (object-value-for-predicate "vcard:fn" triples)))
    (let ((description (object-value-for-predicate "dct:description" triples))
          (parameters (collect-object-for-predicate "ext:queryParameters" triples))
          (query (object-value-for-predicate "ext:definedBy" triples)))
      (make-instance 'odrl:party-collection
                     :uri uri
                     :name name
                     :description description
                     :parameters parameters
                     ;; TODO(B): this escaping should probably be applied to all strings
                     :query (cl-ppcre:regex-replace-all "\"" query "\\\"")))))

;; TODO: Should be able to handle prefixes collection URIs?
(defun retrieve-assets-for-collection (collection)
  "Retrieve a list of assets that are part of the COLLECTION."
  (mapcar
   (lambda (obj) (jsown:val (jsown:val obj "asset") "value"))
   (sparql:select
    (format nil "DISTINCT ?asset")
    (format nil "?asset a odrl:Asset ; odrl:partOf ~a ." (escape-uri collection)))))

(defun make-asset-collection (triple)
  "Make an `odrl:asset-collection' instance for the target linked in TRIPLE."
  (when-let* ((uri (object-value-from-triple triple))
              (triples (retrieve-triples uri))
              (name (object-value-for-predicate "vcard:fn" triples))
              (graph (object-value-for-predicate "ext:graphPrefix" triples))
              (assets (retrieve-assets-for-collection uri)))
    (let ((description (object-value-for-predicate "dct:description" triples)))
      (make-instance 'odrl:asset-collection
                     :uri uri
                     :name name
                     :description description
                     :graph graph
                     :assets (mapcar #'make-node-shape assets)))))

(defun make-node-shape (uri)
  "Make a `shacl:node-shape' for the blank node with the given URI."
  (when-let* ((triples (retrieve-triples uri))
              (target-class (object-value-for-predicate "sh:targetClass" triples)))
    (let* ((not-constraint (find-triple-for-predicate "sh:not" triples))
           (properties (if not-constraint
                           (let* ((not-uri (object-value-from-triple not-constraint))
                                  (triples-in-not (retrieve-triples not-uri)))
                             (filter-triples-for-predicate "sh:property" triples-in-not))
                           (filter-triples-for-predicate "sh:property" triples))))
      (make-instance 'shacl:node-shape
                     :uri uri
                     :target-class target-class
                     :properties (mapcar #'make-property-shape properties)
                     :notp (when not-constraint t)))))

(defun blank-node-uri-p (uri)
  "Check whether a given URI is for a blank."
  ;; TODO(C): match on alphanumeric characters in id part
  (cl-ppcre:scan "<?http://lblod.data.gift/bnode/.+>?" uri))

(defun make-property-shape (triple)
  "Make a `shacl::property-shape' from the provided TRIPLE."
  (when-let* ((uri (object-value-from-triple triple))
              (triples (retrieve-triples uri))
              (path-triple (find-triple-for-predicate "sh:path" triples))
              (path-uri (object-value-from-triple path-triple)))
    (make-instance
     'shacl:property-shape
     :uri uri
     :path (if (blank-node-uri-p path-uri)
               (make-property-path path-uri)
               path-uri))))

(defun make-property-path (uri)
  "Make a `shacl:property-path' from the data linked to the resource with URI."
  (when-let* ((triple (car (retrieve-triples uri)))
              (predicate-path (predicate-value-from-triple triple))
              (object (object-value-from-triple triple)))
    (make-instance 'shacl:property-path
                    :predicate-path predicate-path
                    :object object)))
