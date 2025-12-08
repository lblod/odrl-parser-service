(in-package :odrl-parser)

;; Read policy files
;;
;; Read an ODRL policy specified as n-triples in a file and insert the triples into the backend.

(defun policy-file (&optional filename)
  "Get the path to the file to read the ODRL policy from.

If FILENAME is nil, fall back to the \"config\" as default name for the policy file."
  (if (find :docker *features*)
      (concatenate 'string "../config/" (or filename "config") ".nt")
      "examples/config-simplified.nt"))

(defun load-policy-file (&optional filename)
  "Load the ODRL policy from `policy-file' and insert the triples in the triplestore."
  (handler-case
      (let* ((path (policy-file filename))
             (string-content (read-ntriples-file path))
             (parsed-content (nt:parse-nt string-content))
             (triples (mapcar #'triple-to-string parsed-content)))
        ;; TODO(C): look into usage of `without-update-group' macro
        (sparql:insert (apply #'concatenate 'string triples))
        (format t "~& >> INFO: Loaded policy from ~A" path)
        (load-prefixes-from-file filename))
    (error (e)
      (format t "~& >> WARN: An error occurred when trying to read the configuration file: ~% >>>> '~A'~%" e))))

(defun read-ntriples-file (path)
  "Read the n-triples file `policy-file' and return its contents as a single string."
  (let ((path (asdf:system-relative-pathname :odrl-parser path)))
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


;; NOTE (05/12/2025): The following functionality load a set of prefixes from a ttl file. The
;; assumption is that the ntriples file read with above functionality was generated from the ttl
;; file with the same name, and that this ttl file thus contains all necessary prefixes (or at least
;; those the author deemed appropriate to define). This is a bit of a hacky way to get the necessary
;; prefixes without hardcoding them in the service. In the future this service should be able to
;; just read a policy from a ttl file instead of an ntriples file and thus get the "right" prefixes
;; along with that.
(defun prefixes-file (&optional filename)
  "Get the file to read the needed prefixes from."
    (if (find :docker *features*)
      (concatenate 'string "../config/" (or filename "config") ".ttl")
      "examples/config-simplified.ttl"))

(defparameter prefix-declaration-regex
  "@prefix +([a-zA-Z0-9_-]+): +<([a-zA-Z0-9:/#-_]+)> *\."
  "A regex for prefix declarations in TTl.

Warning: this regex is *not* compliant with the TTL standard.  For example, it does not cover all
allowed characters and allows invalid start characters.")

(defun read-prefixes-from-file (file)
  "Read `*prefix-file*' and return its prefix declarations as a list of strings."
  (cl-ppcre:all-matches-as-strings prefix-declaration-regex (alexandria:read-file-into-string file)))

(defun append-prefix-for-declaration (declaration)
  "Extract the prefix label and iri from DECLARATION and use them as arguments for `add-prefix.'"
  (cl-ppcre:register-groups-bind (label iri) (prefix-declaration-regex declaration)
    (add-prefix label iri)))

(defun load-prefixes-from-file (&optional filename)
  "Read all prefixes declared in FILENAME and register them using `add-prefix'."
  (handler-case
      (let* ((path (prefixes-file filename))
             (rel-path (asdf:system-relative-pathname :odrl-parser path)))
        (mapcar #'append-prefix-for-declaration (read-prefixes-from-file rel-path))
        (format t "~& >> INFO: Loaded prefixes from ~A" path))
    (error (e)
      (format t "~& >> WARN: An error occurred when trying to read the prefixes from file: ~% >>>> '~A'~%" e))))
