(in-package :odrl-parser)

;; Read policy files
;;
;; Read an ODRL policy specified as n-triples in a file and insert the triples into the backend.

;; TODO(B): Make exact policy file configurable
(defparameter *policy-file*
  (if (find :docker *features*)
      "../config/config.nt"
      "examples/config-simplified.nt")
  "The file to read the ODRL policy from.")

;; TODO(C): load on service start?
(defun load-policy-file ()
  "Load the ODRL policy from `*policy-file*' and insert the triples in the triplestore."
  (let* ((string-content (read-ntriples-file))
         (parsed-content (nt:parse-nt string-content))
         (triples (mapcar #'triple-to-string parsed-content)))
    ;; TODO(C): look into usage of `without-update-group' macro
    (sparql:insert (apply #'concatenate 'string triples))))

(defun read-ntriples-file ()
  "Read the n-triples file `*policy-file*' and return its contents as a single string."
  (let ((path (asdf:system-relative-pathname :odrl-parser *policy-file*)))
    (alexandria:read-file-into-string path)))

;; Functions to convert the triples returned by cl-ntriples parsing to plain strings that can be
;; used in the body of a SPARQL insert statement.
(defun triple-to-string (triple)
  "Convert a TRIPLE to a string."
  (format nil "~a ~a ~a .~&"
          (subject-to-string (first triple))
          (escape-uri (second triple))
          (object-to-string (third triple))))

(defun subject-to-string (subject)
  "Convert a triple's SUBJECT to an escaped string."
  ;; NOTE (05/09/2025): The script to convert ttl to n-triples already skolemises the data before
  ;; converting it.  Consequently, any n-triples configuration file generated with it should *not*
  ;; contain anymore blank nodes that are merely identified by an alphanumeric id. I kept this
  ;; else-branch in case some n-triples configurations created in some other way are used.
  (if (string= (subseq subject 0 4) "http")
      (escape-uri subject)
      (escape-node-id subject)))

(defun object-to-string (object)
  "Convert a triple's OBJECT to an escaped string."
  (let ((object-uri (getf object :object-uriref))
        (node-id (getf object :object-node-id))
        (literal (getf object :literal-string)))
    (cond
      (object-uri (escape-uri object-uri))
      (node-id (escape-node-id node-id)) ; NOTE (05/09/2025): See note in `subject-to-string'
      (literal (escape-literal object))
      (t (error "Incorrect object: ~a" object)))))

(defun escape-literal (literal)
  (let ((literal-str (getf literal :literal-string))
        (lang (getf literal :lang))
        (type (getf literal :uriref)))
    (s+ (format nil "\"\"\"~a\"\"\"" literal-str)
        (if lang (format nil "@~a" lang) "")
        (if type (format nil "^^~a" (s-url type)) ""))))

(defun escape-node-id (node-id)
  ;; NOTE (05/09/2025): The base path is the same as used by the ttl to n-triples script when
  ;; skolemising its input ttl.
  (s+ "<http://lblod.data.gift/bnode/" node-id ">"))

(defun escape-uri (uri)
  (s-url uri))
