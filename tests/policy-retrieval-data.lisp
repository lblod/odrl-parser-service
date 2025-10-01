(in-package :policy-retrieval)

(defvar test-property-path
  `((:OBJ
     ("s" :OBJ ("type" . "uri")
          ("value"
           . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b12"))
     ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#path"))
     ("o" :OBJ ("type" . "uri")
          ("value" . "http://data.vlaanderen.be/ns/persoon#registratie")))))

(defvar test-party-collection-triples
  '((:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty"))
     ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/2006/vcard/ns#fn"))
     ("o" :OBJ ("type" . "literal") ("value" . "Public user")))
    (:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/definedBy"))
     ("o" :OBJ ("type" . "literal")
      ("value" . "SELECT DISTINCT ?s WHERE { ?s ?p ?o . }")))
    (:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://purl.org/dc/terms/description"))
     ("o" :OBJ ("type" . "literal")
      ("value"
       . "This party represent all (possibly not logged in) users of the system.")))
    (:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
     ("o" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/ns/odrl/2/PartyCollection")))))

(defvar test-party-collection-triples-no-name
  '((:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/definedBy"))
     ("o" :OBJ ("type" . "literal")
      ("value" . "SELECT DISTINCT ?s WHERE { ?s ?p ?o . }")))
    (:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://purl.org/dc/terms/description"))
     ("o" :OBJ ("type" . "literal")
      ("value"
       . "This party represent all (possibly not logged in) users of the system.")))
    (:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
     ("o" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/ns/odrl/2/PartyCollection")))))

(defvar test-asset-collection-no-name
  '((:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value"
       . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
     ("o" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/ns/odrl/2/AssetCollection")))
    (:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value"
       . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/graphPrefix"))
     ("o" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/graphs/organizations/")))
    (:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value"
       . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://purl.org/dc/terms/description"))
     ("o" :OBJ ("type" . "literal")
      ("value"
       . "This asset collection contains all information that is available to users with the LoketLB-mandaatGebruiker role in the context of the LMB app.")))))

(defvar test-asset-collection-no-graph
  '((:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value"
       . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
     ("o" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/ns/odrl/2/AssetCollection")))
    (:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value"
       . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://purl.org/dc/terms/description"))
     ("o" :OBJ ("type" . "literal")
      ("value"
       . "This asset collection contains all information that is available to users with the LoketLB-mandaatGebruiker role in the context of the LMB app.")))
    (:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value"
       . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
     ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/2006/vcard/ns#fn"))
     ("o" :OBJ ("type" . "literal") ("value" . "organization-mandatendatabank")))))

(defvar test-asset-collection-no-name-or-graph
  '((:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value"
       . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
     ("o" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/ns/odrl/2/AssetCollection")))
    (:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value"
       . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://purl.org/dc/terms/description"))
     ("o" :OBJ ("type" . "literal")
      ("value"
       . "This asset collection contains all information that is available to users with the LoketLB-mandaatGebruiker role in the context of the LMB app.")))))

(defvar test-permission-no-action
  '((:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForPublic"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
     ("o" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/ns/odrl/2/Permission")))
    (:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForPublic"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/ns/odrl/2/assignee"))
     ("o" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty")))
    (:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForPublic"))
     ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/target"))
     ("o" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/lmbPublicSlice")))))

(defvar test-policy-without-rules
  '((:OBJ
     ("s" :OBJ ("type" . "uri")
      ("value" . "http://mu.semte.ch/vocabularies/ext/examplePolicyLMB"))
     ("p" :OBJ ("type" . "uri")
      ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
     ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Set")))))

(defvar test-example-policy
  '((:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/policeCouncilParty"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/2006/vcard/ns#fn"))
  ("o" :OBJ ("type" . "literal") ("value" . "Politieraad lezer")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/mandaatGebruikerParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/queryParameters"))
  ("o" :OBJ ("type" . "literal") ("value" . "session_group")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/mandaatGebruikerParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/definedBy"))
  ("o" :OBJ ("type" . "literal")
   ("value" . "
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
  SELECT DISTINCT ?session_group ?session_role WHERE {
    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group ;
                  ext:sessionRole ?session_role .
    FILTER( ?session_role = \"LoketLB-mandaatGebruiker\" )
  }
  ")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/geslachtCodeAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#targetClass"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/GeslachtCode")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b10"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#path"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/viewOnlyModules")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForMandatarisOrg"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/assignee"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/mandaatGebruikerParty")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptSchemeAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/partOf"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbPublicSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptSchemeAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#targetClass"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/2004/02/skos/core#ConceptScheme")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForVendorOnMandatarisOrg"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/assigner"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbSystem")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/personMultipleExcludedAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#NodeShape")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/vendorGebruikerParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/PartyCollection")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbPublicSlice"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/AssetCollection")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowWriteForMandatarisOrg"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/assignee"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/mandaatGebruikerParty")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/fractieAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/partOf"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForPublic"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/target"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbPublicSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/mandatarisAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/partOf"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/administrativeUnitAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#property"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b4")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/personMultipleExcludedAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#not"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b13")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/policeCouncilParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://purl.org/dc/terms/description"))
  ("o" :OBJ ("type" . "literal")
   ("value"
    . "This party collection represents all users who can access the information regarding police council mandates. This is defined by the members of communes that share the control of this police council.")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/geslachtCodeAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#NodeShape")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/policeCouncilParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/PartyCollection")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForPoliceCouncilOnMandatarisOrg"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/assigner"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbSystem")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#property"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b6")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptSchemeAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#property"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b2")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForMandatarisOrg"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/assigner"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbSystem")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/2006/vcard/ns#fn"))
  ("o" :OBJ ("type" . "literal") ("value" . "organization-mandatendatabank")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowWriteForMandatarisOrg"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/assigner"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbSystem")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b13"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#property"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b15")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptSchemeAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#property"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b3")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#property"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b8")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/personAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Asset")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForPoliceCouncilOnMandatarisOrg"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/assignee"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/policeCouncilParty")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForAuthenticatedOnViewOnlyModules"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/assignee"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/authenticatedParty")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/administrativeUnitViewOnlyAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#property"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b10")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b16"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#property"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b17")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForVendorOnMandatarisOrg"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/assignee"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/vendorGebruikerParty")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForPublic"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/assigner"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbSystem")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/administrativeUnitAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#NodeShape")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbSystem"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/2006/vcard/ns#fn"))
  ("o" :OBJ ("type" . "literal") ("value" . "LMB System")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/2006/vcard/ns#fn"))
  ("o" :OBJ ("type" . "literal") ("value" . "Public user")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbSystem"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://purl.org/dc/terms/description"))
  ("o" :OBJ ("type" . "literal")
   ("value"
    . "The LMB system party used as an assigner of the permission in this profile")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/personAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#targetClass"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://data.vlaanderen.be/ns/persoon#Persoon")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForPublic"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/Permission")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/administrativeUnitViewOnlyAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#NodeShape")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbPublicSlice"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/2006/vcard/ns#fn"))
  ("o" :OBJ ("type" . "literal") ("value" . "public")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/mandaatGebruikerParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://purl.org/dc/terms/description"))
  ("o" :OBJ ("type" . "literal")
   ("value"
    . "This party collection represents all users who received the LoketLB-mandaatGebruiker role through ACM/IDM")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/authenticatedParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://purl.org/dc/terms/description"))
  ("o" :OBJ ("type" . "literal")
   ("value" . "This represents all logged in users of the system.")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b17"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#path"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b18")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptSchemeAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#NodeShape")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/fractieAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#NodeShape")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/vendorGebruikerParty"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/2006/vcard/ns#fn"))
  ("o" :OBJ ("type" . "literal") ("value" . "Vendor users")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/mandatarisAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#NodeShape")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowWriteForMandatarisOrg"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/Permission")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbAuthenticatedPublicSlice"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/2006/vcard/ns#fn"))
  ("o" :OBJ ("type" . "literal") ("value" . "view-only-modules")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b6"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#path"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b7")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForMandatarisOrg"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/Permission")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b13"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#property"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b14")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/AssetCollection")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/graphPrefix"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/graphs/organizations/")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b1"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#path"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/2004/02/skos/core#inScheme")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/examplePolicyLMB"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/permission"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForVendorOnMandatarisOrg")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/examplePolicyLMB"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/permission"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForPoliceCouncilOnMandatarisOrg")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b9"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#inversePath"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/vocabularies/lmb/InstallatievergaderingStatus")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/examplePolicyLMB"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/permission"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForAuthenticatedOnViewOnlyModules")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbAuthenticatedPublicSlice"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/AssetCollection")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForPoliceCouncilOnMandatarisOrg"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/Permission")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/examplePolicyLMB"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Set")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/personInversPathAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Asset")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/examplePolicyLMB"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/permission"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForPublic")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/personAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/partOf"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/examplePolicyLMB"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/permission"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForMandatarisOrg")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#NodeShape")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/policeCouncilParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/definedBy"))
  ("o" :OBJ ("type" . "literal")
   ("value" . "
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
  SELECT DISTINCT ?session_group ?session_role WHERE {
    <SESSION_ID> ext:sessionGroup ?original_session_group ;
                  ext:sessionRole ?session_role .
    ?original_session_group ext:deeltBestuurVan/mu:uuid ?session_group .

    FILTER( ?session_role = \"LoketLB-mandaatGebruiker\" )
  }
  ")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForVendorOnMandatarisOrg"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/Permission")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/examplePolicyLMB"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/permission"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowWriteForMandatarisOrg")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbAuthenticatedPublicSlice"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/graphPrefix"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/graphs/authenticated/public")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/definedBy"))
  ("o" :OBJ ("type" . "literal")
   ("value" . "
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX muAccount: <http://mu.semte.ch/vocabularies/account/>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
  SELECT DISTINCT * WHERE {
    VALUES ?session {
      <SESSION_ID>
    }
    ?session a ?thing .
  }
  ")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b15"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#path"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://data.vlaanderen.be/ns/persoon#heeftGeboorte")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b8"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#path"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b9")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/personInversPathAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/partOf"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForAuthenticatedOnViewOnlyModules"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/Permission")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/examplePolicyLMB"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/profile"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/muAuthProfile")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b5"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#inversePath"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/all")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/authenticatedParty"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/2006/vcard/ns#fn"))
  ("o" :OBJ ("type" . "literal") ("value" . "Authenticated user")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/queryParameters"))
  ("o" :OBJ ("type" . "literal") ("value" . "session_role")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/personAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#NodeShape")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/administrativeUnitAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Asset")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbSystem"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Party")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/administrativeUnitViewOnlyAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Asset")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/queryParameters"))
  ("o" :OBJ ("type" . "literal") ("value" . "session_group")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptSchemeAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Asset")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/personInversPathAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#targetClass"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://data.vlaanderen.be/ns/persoon#Persoon")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/personMultipleExcludedAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Asset")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/authenticatedParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/definedBy"))
  ("o" :OBJ ("type" . "literal")
   ("value" . "
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
  SELECT DISTINCT ?session_group ?session_role WHERE {
    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group ;
                 ext:sessionRole ?session_role .
  }
  ")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/administrativeUnitViewOnlyAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/partOf"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbAuthenticatedPublicSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/geslachtCodeAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Asset")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/partOf"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbPublicSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/vendorGebruikerParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/definedBy"))
  ("o" :OBJ ("type" . "literal")
   ("value" . "
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
  ")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b3"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#path"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b11"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#property"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b12")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/personMultipleExcludedAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#targetClass"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://data.vlaanderen.be/ns/persoon#Persoon")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/vendorGebruikerParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/queryParameters"))
  ("o" :OBJ ("type" . "literal") ("value" . "session_role")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://purl.org/dc/terms/description"))
  ("o" :OBJ ("type" . "literal")
   ("value"
    . "This party represent all (possibly not logged in) users of the system.")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/fractieAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#targetClass"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://data.vlaanderen.be/ns/mandaat#Fractie")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://purl.org/dc/terms/description"))
  ("o" :OBJ ("type" . "literal")
   ("value"
    . "This asset collection contains all information that is available to users with the LoketLB-mandaatGebruiker role in the context of the LMB app.")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b12"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#path"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://data.vlaanderen.be/ns/persoon#registratie")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/policeCouncilParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/queryParameters"))
  ("o" :OBJ ("type" . "literal") ("value" . "session_role")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/authenticatedParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/PartyCollection")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b7"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#inversePath"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/regorg#orgStatus")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForVendorOnMandatarisOrg"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/target"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbPublicSlice"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://purl.org/dc/terms/description"))
  ("o" :OBJ ("type" . "literal")
   ("value"
    . "This asset collection contains all information that is available to the public in the context of the LMB app.")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForPublic"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/action"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/read")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/mandaatGebruikerParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/PartyCollection")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptSchemeAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#property"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b1")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b14"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#path"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://data.vlaanderen.be/ns/persoon#registratie")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForPoliceCouncilOnMandatarisOrg"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/target"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/mandatarisAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#targetClass"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://data.vlaanderen.be/ns/mandaat#Mandataris")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/vendorGebruikerParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/queryParameters"))
  ("o" :OBJ ("type" . "literal") ("value" . "session_group")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/mandaatGebruikerParty"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/2006/vcard/ns#fn"))
  ("o" :OBJ ("type" . "literal") ("value" . "Mandaten users")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForAuthenticatedOnViewOnlyModules"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/target"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbAuthenticatedPublicSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#targetClass"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/2004/02/skos/core#Concept")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/fractieAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Asset")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowWriteForMandatarisOrg"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/action"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/modify")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/policeCouncilParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/queryParameters"))
  ("o" :OBJ ("type" . "literal") ("value" . "session_group")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/personMultipleExcludedAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/partOf"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbPublicSlice"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/graphPrefix"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://mu.semte.ch/graphs/public")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForMandatarisOrg"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/action"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/read")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/vendorGebruikerParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://purl.org/dc/terms/description"))
  ("o" :OBJ ("type" . "literal")
   ("value"
    . "This party collection represents all users who can access the information regarding communes or OCMWs through their vendor api key")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b18"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#inversePath"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://data.vlaanderen.be/ns/mandaat#isBestuurlijkeAliasVan")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbAuthenticatedPublicSlice"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://purl.org/dc/terms/description"))
  ("o" :OBJ ("type" . "literal")
   ("value"
    . "This asset collection contains all information that is available to all authenticated users in the context of the LMB app.")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/geslachtCodeAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/partOf"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbPublicSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/mandatarisAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Asset")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/mandaatGebruikerParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/queryParameters"))
  ("o" :OBJ ("type" . "literal") ("value" . "session_role")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForPoliceCouncilOnMandatarisOrg"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/action"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/read")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/PartyCollection")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForVendorOnMandatarisOrg"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/action"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/read")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b2"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#path"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/2004/02/skos/core#prefLabel")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForMandatarisOrg"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/target"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/personAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#not"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b11")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/personInversPathAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#not"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b16")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/conceptAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/Asset")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b4"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/shacl#path"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://lblod.data.gift/bnode/n950739537b524263b0340509e4433910b5")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/administrativeUnitViewOnlyAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#targetClass"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowWriteForMandatarisOrg"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/target"))
  ("o" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/lmbOrganizationMandatesSlice")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value"
    . "http://mu.semte.ch/vocabularies/ext/allowReadForAuthenticatedOnViewOnlyModules"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/action"))
  ("o" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/read")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/personInversPathAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#NodeShape")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/allowReadForPublic"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/odrl/2/assignee"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/publicParty")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/administrativeUnitAsset"))
  ("p" :OBJ ("type" . "uri")
   ("value" . "http://www.w3.org/ns/shacl#targetClass"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid")))
 (:OBJ
  ("s" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/administrativeUnitAsset"))
  ("p" :OBJ ("type" . "uri") ("value" . "http://www.w3.org/ns/odrl/2/partOf"))
  ("o" :OBJ ("type" . "uri")
   ("value" . "http://mu.semte.ch/vocabularies/ext/lmbPublicSlice")))))
