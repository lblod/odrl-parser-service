; Simplified version of the authorisation policy for the LMB app.
; Full version: <https://github.com/lblod/app-lokaal-mandatenbeheer/blob/master/config/cl-authorization/config.lisp>

(in-package :acl)
; left out for simplicity
; (define-graph sessions ("http://mu.semte.ch/graphs/sessions")
;   ("musession:Session" -> _))

; (define-graph impersonating-sessions ("http://mu.semte.ch/graphs/sessions/")
;   ("musession:Session" -> _))

(define-prefixes
  :astreams "http://www.w3.org/ns/activitystreams#"
  :adms "http://www.w3.org/ns/adms#"
  :besluit "http://data.vlaanderen.be/ns/besluit#"
  :contacthub "http://data.lblod.info/vocabularies/contacthub/"
  :dct "http://purl.org/dc/terms/"
  :euvoc "http://publications.europa.eu/ontology/euvoc#"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :eli "http://data.europa.eu/eli/ontology#"
  :foaf "http://xmlns.com/foaf/0.1/"
  :lblodlg "http://data.lblod.info/vocabularies/leidinggevenden/"
  :locn "http://www.w3.org/ns/locn#"
  :m8g "http://data.europa.eu/m8g/"
  :mandaat "http://data.vlaanderen.be/ns/mandaat#"
  :musession "http://mu.semte.ch/vocabularies/session/"
  :nfo "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
  :org "http://www.w3.org/ns/org#"
  :person "http://www.w3.org/ns/person#"
  :persoon "http://data.vlaanderen.be/ns/persoon#"
  :prov "http://www.w3.org/ns/prov#"
  :schema "http://schema.org/"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :extlmb "http://mu.semte.ch/vocabularies/ext/lmb/"
  :form "http://lblod.data.gift/vocabularies/forms/"
  :lmb "http://lblod.data.gift/vocabularies/lmb/"
  :sh "http://www.w3.org/ns/shacl#"
)

(define-graph public ("http://mu.semte.ch/graphs/public")
  ("ext:FileAddress" -> _)
  ("nfo:FileDataObject" -> _)
  ("prov:Location" -> _)
  ("besluit:Bestuurseenheid" -> _)
  ("ext:BestuurseenheidClassificatieCode" -> _)
  ("ext:BestuursorgaanClassificatieCode" -> _)
  ("lmb:Bestuursperiode" -> _)
  ("ext:Fractietype" -> _)
  ("ext:BestuursfunctieCode" -> _)
  ("ext:MandatarisStatusCode" -> _)
  ("ext:BeleidsdomeinCode" -> _)
  ("ext:GeslachtCode" -> _)
  ("euvoc:Country" -> _)
  ("mandaat:RechtstreekseVerkiezing" -> _)
  ("mandaat:Verkiezingsresultaat" -> _)
  ("ext:VerkiezingsresultaatGevolgCode" -> _)
  ("org:Role" -> _)
  ("skos:ConceptScheme" -> _)
  ("skos:Concept" -> _)
  ("foaf:Document" -> _)
  ("ext:DisplayType" -> _)
  ("ext:FormLibraryEntry" -> _)
  ("ext:FormLibrary" -> _)
  ("form:ValidPhoneNumber" -> _)
  ("form:RequiredConstraint" -> _)
  ("lmb:MandatarisPublicationStatusCode" -> _))

(define-graph organization-mandatendatabank ("http://mu.semte.ch/graphs/organizations/")
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" -> _)
  ("adms:Identifier" -> _)
  ("contacthub:AgentInPositie" -> _)
  ("mandaat:Fractie" -> _)
  ("persoon:Geboorte" -> _)
  ("persoon:Overlijden" -> _)
  ("org:Membership" -> _)
  ("besluit:Besluit" -> _)
  ("besluit:Artikel" -> _)
  ("eli:LegalResource" -> _)
  ("besluit:Bestuursorgaan" -> _)
  ("mandaat:Mandataris" -> _)
  ("mandaat:Mandaat" -> _)
  ("ext:BeleidsdomeinCode" -> _)
  ("org:Post" -> _)
  ("person:Person" -> _)
  ("form:Form" -> _)
  ("form:PropertyGroup" -> _)
  ("ext:CustomFormType" -> _)
  ("ext:GeneratedForm" -> _)
  ("form:TopLevelForm" -> _)
  ("form:Extension" -> _)
  ("form:Field" -> _)
  ("form:ValidPhoneNumber" -> _)
  ("form:RequiredConstraint" -> _)
  ("lmb:Installatievergadering" -> _)
  ("lmb:InstallatievergaderingStatus" -> _)
  ("mandaat:RechtstreekseVerkiezing" -> _)
  ("mandaat:Kandidatenlijst" -> _)
  ("ext:KandidatenlijstLijsttype" -> _)
  ("mandaat:Verkiezingsresultaat" -> _)
  ("ext:SystemNotification" -> _)
  ("astreams:Tombstone" -> _)
  ("ext:BestuurseenheidContact" -> _)
  ("ext:VerkiezingsresultaatGevolgCode" -> _)
  ("schema:ContactPoint" -> _)
  ("locn:Address" -> _)
  ("skos:ConceptScheme" -> _)
  ("skos:Concept" -> _)
  ("sh:ValidationResult" -> _)
  ("sh:ValidationReport" -> _)
  ("ext:ReportStatus" -> _))

(define-graph view-only-modules ("http://mu.semte.ch/graphs/authenticated/public")
  ("besluit:Bestuurseenheid" -> "ext:viewOnlyModules"))

(supply-allowed-group "public")

(supply-allowed-group "authenticated"
  :parameters ()
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group ?session_role WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                         ext:sessionRole ?session_role.
          }")

(supply-allowed-group "vendor"
  :parameters ("session_group" "session_role")
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX muAccount: <http://mu.semte.ch/vocabularies/account/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group ?session_role WHERE {
            VALUES ?session {
              <SESSION_ID>
            }
            {{
              ?session muAccount:canActOnBehalfOf/mu:uuid ?session_group;
                           muAccount:account/ext:sessionRole ?session_role.
            } UNION {
              ?session muAccount:account ?account.
              ?session muAccount:canActOnBehalfOf/ext:isOCMWVoor/mu:uuid ?session_group ;
                                      muAccount:account/ext:sessionRole ?session_role.
              ?session muAccount:canActOnBehalfOf/ext:isOCMWVoor/^<http://lblod.data.gift/vocabularies/lmb/heeftBestuurseenheid>/<http://lblod.data.gift/vocabularies/lmb/hasStatus> <http://data.lblod.info/id/concept/InstallatievergaderingStatus/a40b8f8a-8de2-4710-8d9b-3fc43a4b740e> .
              VALUES ?account {
                <http://data.lblod.info/vendors/14db001d-ea0f-4a8a-8453-c48547347588> # Cipal
                <http://data.lblod.info/vendors/42edb420-08c7-4ede-9961-bc0e527d0f3b> # Green Valley
                <http://data.lblod.info/vendors/dc62419e-1267-44e7-9562-0114e2708b6f> # Remmicom
              }
            }}
          }")


(supply-allowed-group "mandaat-gebruiker"
  :parameters ("session_group" "session_role")
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group ?session_role WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                         ext:sessionRole ?session_role.
            FILTER( ?session_role = \"LoketLB-mandaatGebruiker\" )
          }")

(supply-allowed-group "politieraad-lezer"
  :parameters ("session_group" "session_role")
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group ?session_role WHERE {
            <SESSION_ID> ext:sessionGroup ?original_session_group;
                         ext:sessionRole ?session_role.
            ?original_session_group ext:deeltBestuurVan/mu:uuid ?session_group.

            FILTER( ?session_role = \"LoketLB-mandaatGebruiker\" )
          }")

(grant (read)
       :to-graph public
       :for-allowed-group "public")

(grant (read)
       :to-graph view-only-modules
       :for-allowed-group "authenticated")

(grant (read)
       :to-graph organization-mandatendatabank
       :for-allowed-group "vendor")

(grant (read)
        :to-graph organization-mandatendatabank
        :for-allowed-group "politieraad-lezer")

(grant (read write)
       :to-graph organization-mandatendatabank
       :for-allowed-group "mandaat-gebruiker")
