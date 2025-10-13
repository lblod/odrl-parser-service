(asdf:defsystem :odrl-parser
  :name "odrl-parser"
  :description "Load and parse ODRL policies in a semantic.works stack."
  :version "0.0.1"
  :license "MIT"
  :depends-on (mu-support cl-fuseki dexador cl-ntriples trivial-signal)
  :components ((:file packages)
               (:file settings)
               (:file policy-retrieval)
               (:file acl)
               (:file shacl)
               (:file odrl)
               (:file odrl-parser)
               (:file query-execution)
               (:file configuration-parsing)))
