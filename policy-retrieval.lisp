(in-package :policy-retrieval)

;; Functionality to read ODRL policies form the backend an convert them to the service's internal
;; ODRL model.
;;
;; NOTE (13/09/2025): This is currently in a very rough state and requires at least to following
;; improvements:
;; - Catch errors from instance creation. If the retrieved data is incorrect/incomplete, errors can
;;   be thrown by the `make-instance' calls, currently these are never caught and will just
;;   propagate to the top-level.
;; - Improve the overall quality and robustness of the code.
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

(defparameter predicates-plist
  '(:dcterms-description "http://purl.org/dc/terms/description"
    :ext-defined-by "http://mu.semte.ch/vocabularies/ext/definedBy"
    :ext-graph-prefix "http://mu.semte.ch/vocabularies/ext/graphPrefix"
    :ext-query-parameters "http://mu.semte.ch/vocabularies/ext/queryParameters"
    :odrl-action "http://www.w3.org/ns/odrl/2/action"
    :odrl-assignee "http://www.w3.org/ns/odrl/2/assignee"
    :odrl-assigner "http://www.w3.org/ns/odrl/2/assigner"
    :odrl-part-of "http://www.w3.org/ns/odrl/2/partOf"
    :odrl-permission "http://www.w3.org/ns/odrl/2/permission"
    :odrl-profile "http://www.w3.org/ns/odrl/2/profile"
    :odrl-target "http://www.w3.org/ns/odrl/2/target"
    :rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    :sh-inverse-path "http://www.w3.org/ns/shacl#inversePath"
    :sh-not "http://www.w3.org/ns/shacl#not"
    :sh-path "http://www.w3.org/ns/shacl#path"
    :sh-property "http://www.w3.org/ns/shacl#property"
    :sh-target-class "http://www.w3.org/ns/shacl#targetClass"
    :vcard-fn "http://www.w3.org/2006/vcard/ns#fn")
  "A plist containing the full uris for the predicates that are used in ODRL policies.")

(defparameter resource-types-plist
  '(:odrl-asset "http://www.w3.org/ns/odrl/2/Asset"
    :odrl-asset-collection "http://www.w3.org/ns/odrl/2/AssetCollection"
    :odrl-party "http://www.w3.org/ns/odrl/2/Party"
    :odrl-party-collection "http://www.w3.org/ns/odrl/2/PartyCollection"
    :odrl-permission "http://www.w3.org/ns/odrl/2/Permission"
    :odrl-profile "http://www.w3.org/ns/odrl/2/Profile"
    :odrl-set "http://www.w3.org/ns/odrl/2/Set"
    :sh-node-shape "http://www.w3.org/ns/shacl#NodeShape"
    :sh-property-shape "http://www.w3.org/ns/shacl#PropertyShape")
  "A plist containing the full uris for the resources types used in ODRL policies.")

(defun predicate-uri (indicator)
  "Return a string containing the full uri for the predicate matching INDICATOR."
  (getf predicates-plist indicator))

(defun type-uri (indicator)
  "Return a string containing the full uri for the resource type matching INDICATOR."
  (getf resource-types-plist indicator))

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

(defun query-for-policies ()
  "Retrieve a list of the URIs of all ODRL policies."
  (sparql:select
   (format nil "DISTINCT ~a" (mu:s-var "policy"))
   (format nil "~a a ~a ." (s-var "policy") (escape-uri (type-uri :odrl-set)))))

(defun list-known-policies ()
  "Construct a list of the resource URIs all ODRL policies found in the backend."
  (mapcar
   (lambda (policy) (jsown:val (jsown:val policy "policy") "value"))
   (query-for-policies)))

;; TODO: Optimise/generalise query
(defun query-policy-data (policy)
  "Generate a construct query to retrieve all triples for POLICY."
  (format
   nil
   "CONSTRUCT {
      ?s ?p ?o .
    } WHERE {
      BIND (~a as ?policy)
      {
        SELECT DISTINCT ?s ?p ?o
        WHERE {
          GRAPH <http://mu.semte.ch/graphs/odrl-policy> {
            {
              # Policy itself
              ?s a odrl:Set ;
                 ?p ?o .
              FILTER (?s = ?policy)
            } UNION {
              # Permissions
              ?policy odrl:permission ?s .
              ?s a odrl:Permission ;
                 ?p ?o .
            } UNION {
              # Party (Collection)
              ?policy odrl:permission ?permission .
              ?permission odrl:assignee|odrl:assigner ?s .
              ?s ?p ?o .
            } UNION {
              # Asset collections
              ?policy odrl:permission/odrl:target ?s .
              ?s ?p ?o .
            } UNION {
              # Assets (Node shapes)
              ?policy odrl:permission/odrl:target/^odrl:partOf ?s .
              ?s a odrl:Asset ;
                 ?p ?o .
            } UNION {
              # sh:not in node shape
              ?policy odrl:permission/odrl:target/^odrl:partOf/sh:not ?s .
              ?s ?p ?o .
            } UNION {
              # Property shapes in assets
              ?policy odrl:permission/odrl:target/^odrl:partOf/sh:not?/sh:property ?s .
              ?s ?p ?o .
            } UNION {
              # Property paths in property shapes
              ?policy odrl:permission/odrl:target/^odrl:partOf/sh:not?/sh:property/sh:path ?s .
              ?s ?p ?o .
            } UNION {
              # Inverse path in property paths
              ?policy odrl:permission/odrl:target/^odrl:partOf/sh:not?/sh:property/sh:path/sh:inversePath ?s .
              ?s ?p ?o .
            }
          }
        }
      }
    }"
   policy))

(defun retrieve-policy (policy-uri)
  "Retrieve all triples for a policy resource identified by POLICY-URI.

POLICY must either be the full uri or a prefixed uri of the ODRL Set resource.
The result is an RDF graph encoded as jsown objects for the triples.  Each triple has values for the
variables \"s\", \"p\", and \"o\"."
  (let ((uri (escape-uri (expand-uri policy-uri))))
    (sparql:query (query-policy-data uri))))

(defun parse-stored-policy (uri)
  "Retrieve a policy resource URI from the backend and parse it into object instances."
  (let ((triples (retrieve-policy uri)))
    (make-rule-set uri triples)))

;;
;; Parse jsown objects
;;
(defun value-from-object (obj)
  "Get the value given to the \"value\" keyword in OBJ."
  (jsown:val-safe obj "value"))

(defun triple-subject (triple)
  "Return the jsown object that is the value for the subject of TRIPLE."
  (jsown:val-safe triple "s"))

(defun triple-subject-value (triple)
  "Return the jsown object that is the value for the subject of TRIPLE."
  (value-from-object (triple-subject triple)))

(defun triple-predicate (triple)
  "Return the jsown object that is the value for the predicate of TRIPLE."
  (jsown:val-safe triple "p"))

(defun triple-predicate-value (triple)
  "Return the jsown object that is the value for the predicate of TRIPLE."
  (value-from-object (triple-predicate triple)))

(defun triple-object (triple)
  "Return the jsown object that is the value for the object of TRIPLE."
  (jsown:val-safe triple "o"))

(defun triple-object-value (triple)
  "Return the jsown object that is the value for the object of TRIPLE."
  (value-from-object (triple-object triple)))

(defun triples-for-predicate (predicate triples)
  "Return all elements in TRIPLES that have PREDICATE as predicate value."
  (remove-if-not
   (lambda (triple) (string= predicate (triple-predicate-value triple)))
   triples))

(defun triples-for-resource (resource triples)
  "Return an triple objects in TRIPLES that have RESOURCE as subject."
  (remove-if-not
   (lambda (triple) (string= resource (triple-subject-value triple)))
   triples))

(defun triples-for-resource-predicate (resource predicate triples)
  "Return all triple objects in TRIPLES that have RESOURCE as subject and PREDICATE as predicate."
  (remove-if-not
   (lambda (triple)
     (and (string= resource (triple-subject-value triple))
          (string= predicate (triple-predicate-value triple))))
   triples))

(defun triples-for-predicate-object (predicate object triples)
  "Return all triple objects in TRIPLES that have PREDICATE as predicate and OBJECT as object."
  (remove-if-not
   (lambda (triple)
     (and (string= predicate (triple-predicate-value triple))
          (string= object (triple-object-value triple))))
   triples))

(defun list-parts-in-collection (uri triples)
  "Return a list of the uris of all resources that are a part of the collection resource URI in TRIPLES."
  (let ((parts (triples-for-predicate-object (predicate-uri :odrl-part-of) uri triples)))
    (mapcar #'triple-subject-value parts)))

(defun filter-resources-for-type (type triples)
  "Filter the type triples for resources of TYPE in TRIPLES.

TYPE should be a string containing a uri for a resource type."
  (remove-if-not
   (lambda (triple) (string= type (triple-object-value triple)))
   (triples-for-predicate (predicate-uri :rdf-type) triples)))

(defun list-resource-uris (type triples)
  "Return a list containing the uri of each resource of TYPE in TRIPLES."
  (mapcar #'triple-subject-value (filter-resources-for-type type triples)))

(defun list-party-collections (triples)
  "List the uris for ODRL party collection resources in TRIPLES."
  (list-resource-uris (type-uri :odrl-party-collection) triples))

(defun list-asset-collections (triples)
  "List the uris for ODRL asset collection resources in TRIPLES."
  (list-resource-uris (type-uri :odrl-asset-collection) triples))

(defun list-assets (triples)
  "List the uris for ODRL asset resources in TRIPLES."
  (list-resource-uris (type-uri :odrl-asset) triples))

(defun list-permissions-in-policy (triples)
  "Return a list of the uris of all permissions in the policy defined by TRIPLES."
  (mapcar
   (lambda (triple) (triple-object-value triple))
   (triples-for-predicate (predicate-uri :odrl-permission) triples)))

;; NOTE (01/10/2025): These macros are use to make the init-forms in the `let' operators in the
;; conversion functions more readable.
(defmacro first-value-for-predicate (predicate triples)
  "Return the value of the first object for PREDICATE encountered in TRIPLES."
  `(triple-object-value (car (triples-for-predicate ,predicate ,triples))))

(defmacro first-triple-for-resource (uri triples)
  "Return the first triple with URI as subject in TRIPLES."
  `(car (triples-for-resource ,uri ,triples)))

;;
;; Conversion to ODRL
;;
(defun find-concept-with-uri (uri concepts)
  "Find the concept instance in CONCEPTS that has URI as value for its uri slot."
  (when uri
    (find-if
     (lambda (concept) (string= (slot-value concept 'odrl::uri) uri))
     concepts)))

(defun find-shape-with-uri (uri shapes)
  "Find the shape instance in SHAPES that has URI as value for its uri slot."
  (when uri
    (find-if
     (lambda (shape) (string= (slot-value shape 'shacl::uri) uri))
     shapes)))

(defun make-rule-set (uri triples)
  "Make an `odrl:rule-set' instance for the resource with URI."
  (let ((asset-collections (make-asset-collections triples))
        (party-collections (make-party-collections triples)))
    (make-instance
     'odrl:rule-set
     :uri uri
     :rules (mapcar
             (lambda (permission)
               (make-permission permission asset-collections party-collections triples))
             (list-permissions-in-policy triples)))))

(defun make-party-collections (triples)
  "Make an `odrl:party-collection' for each party collection resource in TRIPLES."
  (mapcar
   (lambda (uri) (make-party-collection uri triples))
   (list-party-collections triples)))

(defun make-party-collection (uri policy-triples)
  "Make an `odrl:party-collection' instance for the resource with URI."
  (let* ((triples (triples-for-resource uri policy-triples))
         (name (first-value-for-predicate (predicate-uri :vcard-fn) triples))
         (description (first-value-for-predicate (predicate-uri :dcterms-description) triples))
         (parameters (triples-for-predicate (predicate-uri :ext-query-parameters) triples))
         (query (first-value-for-predicate (predicate-uri :ext-defined-by) triples)))
    (make-instance
     'odrl:party-collection
     :uri uri
     :name name
     :description description
     :parameters (mapcar #'triple-object-value parameters)
     ;; TODO(B): this escaping should probably be applied to all strings
     :query (when query (cl-ppcre:regex-replace-all "\"" query "\\\"")))))

(defun make-asset-collections (triples)
  "Make an `odrl:asset-collection' for each asset collection resource in TRIPLES."
  (let ((assets (make-node-shapes triples)))
    (mapcar
     (lambda (uri) (make-asset-collection uri assets triples))
     (list-asset-collections triples))))

(defun make-asset-collection (uri assets policy-triples)
  "Make an `odrl:asset-collection' instance for the resource with URI."
  (let* ((triples (triples-for-resource uri policy-triples))
         (name (first-value-for-predicate (predicate-uri :vcard-fn) triples))
         (description (first-value-for-predicate (predicate-uri :dcterms-description) triples))
         (graph (first-value-for-predicate (predicate-uri :ext-graph-prefix) triples))
         (assets-in-collection (list-parts-in-collection uri policy-triples)))
    (make-instance
     'odrl:asset-collection
     :uri uri
     :name name
     :description description
     :graph graph
     :assets (mapcar
              (lambda (uri) (find-shape-with-uri uri assets))
              assets-in-collection))))

(defun make-node-shapes (triples)
  "Make a `shacl:node-shape' instance for each ODRL asset resource in triples."
  (mapcar
   (lambda (uri) (make-node-shape uri triples))
   (list-assets triples)))

(defun make-node-shape (uri policy-triples)
  "Make a `shacl:node-shape' for the resource with URI."
  (let* ((triples (triples-for-resource uri policy-triples))
         (target (first-value-for-predicate (predicate-uri :sh-target-class) triples))
         ;; NOTE (01/10/2025): Node shapes may surround their property shapes with a "sh:not"
         ;; constraint component.  The `not-triple' will have a non-nil value if that is the case,
         ;; otherwise it will be nill.  This is used in `properties' to determine whether one has to
         ;; go passed an additional blank node or not to find the properties in a node shape.
         (not-triple (car (triples-for-predicate (predicate-uri :sh-not) triples)))
         (properties (if not-triple
                         (triples-for-resource-predicate
                          (triple-object-value not-triple)
                          (predicate-uri :sh-property)
                          policy-triples)
                         (triples-for-predicate (predicate-uri :sh-property) triples))))
    (make-instance
     'shacl:node-shape
     :uri uri
     :target-class target
     :properties (mapcar
                  (lambda (uri) (make-property-shape uri policy-triples))
                  (mapcar #'triple-object-value properties))
     :notp (when not-triple t))))

(defun blank-node-uri-p (uri)
  "Check whether a given URI is for a blank."
  ;; TODO(C): match on alphanumeric characters in id part
  (cl-ppcre:scan "<?http://lblod.data.gift/bnode/.+>?" uri))

(defun make-property-shape (uri policy-triples)
  "Make a `shacl:property-shape' instance for the resource with URI."
  (let ((path (triple-object-value (first-triple-for-resource uri policy-triples))))
    (make-instance
     'shacl:property-shape
     :uri uri
     :path (if (blank-node-uri-p path)
               (make-property-path path policy-triples)
               path))))

(defun make-property-path (uri policy-triples)
  "Make a `shacl:property-path' instance for the resource with URI."
  (let ((triple (first-triple-for-resource uri policy-triples)))
    (make-instance
     'shacl:property-path
     :predicate-path (triple-predicate-value triple)
     :object (triple-object-value triple))))

(defun make-permission (uri asset-col party-col policy-triples)
  "Make an `odrl:permission' instance for the resource with URI.

ASSET-COL and PARTY-COL should be lists of, respectively, `odrl:asset-collection' and
`odrl:party-collection' instances with which the created `odrl:permission' instance can be linked."
  (let* ((triples (triples-for-resource uri policy-triples))
         (action (first-value-for-predicate (predicate-uri :odrl-action) triples))
         (target (find-concept-with-uri
                  (first-value-for-predicate (predicate-uri :odrl-target) triples)
                  asset-col))
         (assignee (find-concept-with-uri
                    (first-value-for-predicate (predicate-uri :odrl-assignee) triples)
                    party-col)))
    (make-instance
     'odrl:permission
     :uri uri
     :action (make-action action)
     :target target
     :assignee assignee)))

(defun make-action (uri)
  "Make an `odrl:action' instance for the given URI."
  (make-instance 'odrl:action :uri uri))
