(in-package :acl)

(define-prefixes
  :persoon "http://data.vlaanderen.be/ns/persoon#"
  :regorg "http://www.w3.org/ns/regorg#"
  :besluit "http://data.vlaanderen.be/ns/besluit#"
  :lmb "http://lblod.data.gift/vocabularies/lmb/"
  :sh "http://www.w3.org/ns/shacl#"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :mandaat "http://data.vlaanderen.be/ns/mandaat#"
  :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  :vcard "http://www.w3.org/2006/vcard/ns#"
  :dct "http://purl.org/dc/terms/"
  :odrl "http://www.w3.org/ns/odrl/2/"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :rm "http://mu.semte.ch/vocabularies/logical-delete/"
  :typedLiterals "http://mu.semte.ch/vocabularies/typed-literals/"
  :mu "http://mu.semte.ch/vocabularies/core/"
  :xsd "http://www.w3.org/2001/XMLSchema#"
  :app "http://mu.semte.ch/app/"
  :owl "http://www.w3.org/2002/07/owl#")

;; Graphs
;; This asset collection contains all information that is available to the public in the context of the LMB app.
(define-graph public ("http://mu.semte.ch/graphs/public")
  ("skos:ConceptScheme" -> "skos:prefLabel"
    -> "rdf:type"
    -> "skos:inScheme")
  ("skos:Concept" <- "regorg:orgStatus"
    <- "lmb:InstallatievergaderingStatus")
  ("ext:GeslachtCode" -> _)
  ("besluit:Bestuurseenheid" <- _))

;; This asset collection contains all information that is available to all authenticated users in the context of the LMB app.
(define-graph view-only-modules ("http://mu.semte.ch/graphs/authenticated/public")
  ("besluit:Bestuurseenheid" -> "ext:viewOnlyModules"))

;; This asset collection contains all information that is available to users with the LoketLB-mandaatGebruiker role in the context of the LMB app.
(define-graph organization-mandatendatabank ("http://mu.semte.ch/graphs/organizations/")
  ("mandaat:Fractie" -> _)
  ("mandaat:Mandataris" -> _)
  ("persoon:Persoon" x> "persoon:registratie")
  ("persoon:Persoon" <x "mandaat:isBestuurlijkeAliasVan")
  ("persoon:Persoon" x> "persoon:heeftGeboorte"
    x> "persoon:registratie"))


;; Groups
;; This party collection represents all users who received the LoketLB-mandaatGebruiker role through ACM/IDM
(supply-allowed-group "mandaten-users"
  :parameters ("session_group" "session_role")
  :query "
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
  SELECT DISTINCT ?session_group ?session_role WHERE {
    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group ;
                  ext:sessionRole ?session_role .
    FILTER( ?session_role = \"LoketLB-mandaatGebruiker\" )
  }
  ")

;; This party represent all (possibly not logged in) users of the system.
(supply-allowed-group "public-user"
  :query "
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX muAccount: <http://mu.semte.ch/vocabularies/account/>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
  SELECT DISTINCT * WHERE {
    VALUES ?session {
      <SESSION_ID>
    }
    ?session a ?thing .
  }
  ")

;; This represents all logged in users of the system.
(supply-allowed-group "authenticated-user"
  :query "
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
  SELECT DISTINCT ?session_group ?session_role WHERE {
    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group ;
                 ext:sessionRole ?session_role .
  }
  ")

;; This party collection represents all users who can access the information regarding police council mandates. This is defined by the members of communes that share the control of this police council.
(supply-allowed-group "politieraad-lezer"
  :parameters ("session_role" "session_group")
  :query "
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
  SELECT DISTINCT ?session_group ?session_role WHERE {
    <SESSION_ID> ext:sessionGroup ?original_session_group ;
                  ext:sessionRole ?session_role .
    ?original_session_group ext:deeltBestuurVan/mu:uuid ?session_group .

    FILTER( ?session_role = \"LoketLB-mandaatGebruiker\" )
  }
  ")

;; This party collection represents all users who can access the information regarding communes or OCMWs through their vendor api key
(supply-allowed-group "vendor-users"
  :parameters ("session_role" "session_group")
  :query "
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX muAccount: <http://mu.semte.ch/vocabularies/account/>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
  SELECT DISTINCT ?session_group ?session_role WHERE {
    VALUES ?session {
      <SESSION_ID>
    }
    {{
      ?session muAccount:canActOnBehalfOf/mu:uuid ?session_group ;
                    muAccount:account/ext:sessionRole ?session_role .
    } UNION {
      ?session muAccount:account ?account .
      ?session muAccount:canActOnBehalfOf/ext:isOCMWVoor/mu:uuid ?session_group ;
                              muAccount:account/ext:sessionRole ?session_role .
      ?session muAccount:canActOnBehalfOf/ext:isOCMWVoor/^<http://lblod.data.gift/vocabularies/lmb/heeftBestuurseenheid>/<http://lblod.data.gift/vocabularies/lmb/hasStatus> <http://data.lblod.info/id/concept/InstallatievergaderingStatus/a40b8f8a-8de2-4710-8d9b-3fc43a4b740e> .
      VALUES ?account {
        <http://data.lblod.info/vendors/14db001d-ea0f-4a8a-8453-c48547347588> # Cipal
        <http://data.lblod.info/vendors/42edb420-08c7-4ede-9961-bc0e527d0f3b> # Green Valley
        <http://data.lblod.info/vendors/dc62419e-1267-44e7-9562-0114e2708b6f> # Remmicom
      }
    }}
  }
  ")


;; Grants
(grant (write read)
  :to-graph organization-mandatendatabank
  :for-allowed-group "mandaten-users")

(grant (read)
  :to-graph public
  :for-allowed-group "public-user")

(grant (read)
  :to-graph view-only-modules
  :for-allowed-group "authenticated-user")

(grant (read)
  :to-graph organization-mandatendatabank
  :for-allowed-group "politieraad-lezer")

(grant (read)
  :to-graph organization-mandatendatabank
  :for-allowed-group "vendor-users")

