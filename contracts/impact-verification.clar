;; impact-verification contract
;; This contract creates a system for recording and verifying the environmental impact of sustainability projects.
;; It allows project leaders to submit impact claims which are then verified by designated community validators.
;; The contract maintains an immutable record of verified impacts, calculates impact metrics, and generates
;; sustainability credentials that participants can showcase.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROJECT-NOT-FOUND (err u101))
(define-constant ERR-CLAIM-NOT-FOUND (err u102))
(define-constant ERR-CLAIM-ALREADY-PROCESSED (err u103))
(define-constant ERR-INVALID-IMPACT-TYPE (err u104))
(define-constant ERR-INVALID-AMOUNT (err u105))
(define-constant ERR-NOT-ENOUGH-VERIFICATIONS (err u106))
(define-constant ERR-ALREADY-VERIFIED (err u107))
(define-constant ERR-NOT-VALIDATOR (err u108))
(define-constant ERR-DATA-SOURCE-NOT-AUTHORIZED (err u109))
(define-constant ERR-VERIFICATION-PERIOD-ENDED (err u110))

;; Data space definitions

;; Track contract administrator
(define-data-var contract-admin principal tx-sender)

;; Valid impact types and their carbon equivalents (in grams of CO2 per unit)
;; For example: trees planted, water purified in liters, waste recycled in kg
(define-map impact-types 
  { impact-type: (string-ascii 24) }
  { 
    carbon-equivalent: uint, 
    unit: (string-ascii 12), 
    active: bool 
  }
)

;; Registered projects
(define-map projects 
  { project-id: uint }
  {
    owner: principal,
    name: (string-utf8 50),
    description: (string-utf8 500),
    location: (string-utf8 100),
    created-at: uint,
    total-verified-impact: uint,  ;; in carbon equivalent (grams of CO2)
    status: (string-ascii 10)     ;; "active" or "completed"
  }
)

;; Project validators - principals authorized to verify impact claims for a project
(define-map project-validators
  { project-id: uint, validator: principal }
  { 
    authorized-at: uint,
    authorized-by: principal
  }
)

;; External data sources that can provide verification
(define-map authorized-data-sources
  { source-id: (string-ascii 24) }
  {
    name: (string-utf8 100),
    description: (string-utf8 200),
    interface-principal: principal,
    authorized-at: uint
  }
)

;; Impact claims submitted by project owners
(define-map impact-claims
  { claim-id: uint }
  {
    project-id: uint,
    impact-type: (string-ascii 24),
    amount: uint,
    evidence-url: (string-utf8 200),
    submitted-by: principal,
    submitted-at: uint,
    status: (string-ascii 12),     ;; "pending", "verified", "rejected"
    verification-expiry: uint,     ;; time after which the verification period ends
    verifications-required: uint,  ;; number of verifications needed
    verifications-received: uint,  ;; number of verifications received
    verified-amount: uint          ;; final verified amount (may differ from claimed)
  }
)

;; Individual verifications by validators
(define-map verifications
  { claim-id: uint, validator: principal }
  {
    verified-at: uint,
    approved: bool,
    verified-amount: uint,
    comments: (string-utf8 200)
  }
)

;; External data source verifications
(define-map external-verifications
  { claim-id: uint, source-id: (string-ascii 24) }
  {
    verified-at: uint,
    approved: bool,
    verified-amount: uint,
    verification-data: (string-utf8 500)
  }
)

;; Track impact credentials awarded to participants
(define-map impact-credentials
  { principal: principal, credential-id: uint }
  {
    project-id: uint,
    impact-type: (string-ascii 24),
    amount: uint,
    carbon-equivalent: uint,
    awarded-at: uint,
    claim-id: uint
  }
)

;; Counter variables
(define-data-var next-project-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var next-credential-id uint u1)
(define-data-var total-platform-impact uint u0)  ;; total verified impact across all projects (in carbon equivalent)

;; Private functions

;; Calculate carbon equivalent based on impact type and amount
(define-private (calculate-carbon-equivalent (impact-type (string-ascii 24)) (amount uint))
  (let ((impact-info (unwrap! (map-get? impact-types { impact-type: impact-type }) u0)))
    (if (get active impact-info)
        (* amount (get carbon-equivalent impact-info))
        u0)
  )
)

;; Check if principal is a validator for a project
(define-private (is-project-validator (project-id uint) (validator principal))
  (is-some (map-get? project-validators { project-id: project-id, validator: validator }))
)

;; Update claim status based on verifications
(define-private (update-claim-status (claim-id uint))
  (let (
    (claim (unwrap! (map-get? impact-claims { claim-id: claim-id }) false))
    (project-id (get project-id claim))
    (current-verifications (get verifications-received claim))
    (required-verifications (get verifications-required claim))
    (now-block-height block-height)
  )
    (if (>= current-verifications required-verifications)
      ;; Enough verifications received, mark claim as verified
      (let (
        (project (unwrap! (map-get? projects { project-id: project-id }) false))
        (impact-type (get impact-type claim))
        (verified-amount (get verified-amount claim))
        (carbon-impact (calculate-carbon-equivalent impact-type verified-amount))
        (new-total-project-impact (+ (get total-verified-impact project) carbon-impact))
        (new-total-platform-impact (+ (var-get total-platform-impact) carbon-impact))
      )
        ;; Update claim status
        (map-set impact-claims { claim-id: claim-id }
          (merge claim { status: "verified" }))
        
        ;; Update project's total verified impact
        (map-set projects { project-id: project-id }
          (merge project { total-verified-impact: new-total-project-impact }))
        
        ;; Update platform total impact
        (var-set total-platform-impact new-total-platform-impact)
        
        ;; Issue credential to project owner
        (let (
          (credential-id (var-get next-credential-id))
        )
          (map-set impact-credentials
            { principal: (get owner project), credential-id: credential-id }
            {
              project-id: project-id,
              impact-type: impact-type,
              amount: verified-amount,
              carbon-equivalent: carbon-impact,
              awarded-at: now-block-height,
              claim-id: claim-id
            }
          )
          (var-set next-credential-id (+ credential-id u1))
          true
        )
      )
      ;; Not enough verifications yet
      false
    )
  )
)

;; Read-only functions

;; Get information about a specific impact type
(define-read-only (get-impact-type-info (impact-type (string-ascii 24)))
  (map-get? impact-types { impact-type: impact-type })
)

;; Get project information
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

;; Check if a principal is a project validator
(define-read-only (is-validator (project-id uint) (validator principal))
  (is-some (map-get? project-validators { project-id: project-id, validator: validator }))
)

;; Get impact claim details
(define-read-only (get-impact-claim (claim-id uint))
  (map-get? impact-claims { claim-id: claim-id })
)

;; Get verification details
(define-read-only (get-verification (claim-id uint) (validator principal))
  (map-get? verifications { claim-id: claim-id, validator: validator })
)

;; Get external verification details
(define-read-only (get-external-verification (claim-id uint) (source-id (string-ascii 24)))
  (map-get? external-verifications { claim-id: claim-id, source-id: source-id })
)

;; Get impact credential for a participant
(define-read-only (get-impact-credential (principal principal) (credential-id uint))
  (map-get? impact-credentials { principal: principal, credential-id: credential-id })
)

;; Get total platform impact
(define-read-only (get-total-platform-impact)
  (var-get total-platform-impact)
)

;; Public functions

;; Set admin (only current admin can change)
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
    (var-set contract-admin new-admin)
    (ok true)
  )
)

;; Register or update an impact type (admin only)
(define-public (register-impact-type 
    (impact-type (string-ascii 24)) 
    (carbon-equivalent uint) 
    (unit (string-ascii 12)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
    (map-set impact-types 
      { impact-type: impact-type }
      { 
        carbon-equivalent: carbon-equivalent, 
        unit: unit,
        active: true 
      }
    )
    (ok true)
  )
)

;; Deactivate impact type (admin only)
(define-public (deactivate-impact-type (impact-type (string-ascii 24)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
    (let ((impact-info (unwrap! (map-get? impact-types { impact-type: impact-type }) ERR-INVALID-IMPACT-TYPE)))
      (map-set impact-types 
        { impact-type: impact-type }
        (merge impact-info { active: false })
      )
      (ok true)
    )
  )
)

;; Register a new project
(define-public (register-project 
    (name (string-utf8 50)) 
    (description (string-utf8 500)) 
    (location (string-utf8 100)))
  (let (
    (project-id (var-get next-project-id))
    (now-block-height block-height)
  )
    (map-set projects 
      { project-id: project-id }
      {
        owner: tx-sender,
        name: name,
        description: description,
        location: location,
        created-at: now-block-height,
        total-verified-impact: u0,
        status: "active"
      }
    )
    ;; Add project owner as a validator
    (map-set project-validators
      { project-id: project-id, validator: tx-sender }
      {
        authorized-at: now-block-height,
        authorized-by: tx-sender
      }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

;; Add a validator to a project
(define-public (add-project-validator (project-id uint) (validator principal))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (now-block-height block-height)
  )
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
    (map-set project-validators
      { project-id: project-id, validator: validator }
      {
        authorized-at: now-block-height,
        authorized-by: tx-sender
      }
    )
    (ok true)
  )
)

;; Remove a validator from a project
(define-public (remove-project-validator (project-id uint) (validator principal))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
    (map-delete project-validators { project-id: project-id, validator: validator })
    (ok true)
  )
)

;; Update project status
(define-public (update-project-status (project-id uint) (status (string-ascii 10)))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq status "active") (is-eq status "completed")) (err u111))
    (map-set projects 
      { project-id: project-id }
      (merge project { status: status })
    )
    (ok true)
  )
)

;; Submit an impact claim
(define-public (submit-impact-claim
    (project-id uint)
    (impact-type (string-ascii 24))
    (amount uint)
    (evidence-url (string-utf8 200))
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (impact-info (unwrap! (map-get? impact-types { impact-type: impact-type }) ERR-INVALID-IMPACT-TYPE))
    (claim-id (var-get next-claim-id))
    (now-block-height block-height)
  )
    ;; Validate inputs
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
    (asserts! (get active impact-info) ERR-INVALID-IMPACT-TYPE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> verifications-required u0) (err u112))
    (asserts! (> verification-expiry now-block-height) (err u113))
    
    ;; Create impact claim
    (map-set impact-claims
      { claim-id: claim-id }
      {
        project-id: project-id,
        impact-type: impact-type,
        amount: amount,
        evidence-url: evidence-url,
        submitted-by: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0,
        verified-amount: u0
      }
    )
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify an impact claim as a validator
(define-public (verify-impact-claim
    (claim-id uint)
    (approved bool)
    (verified-amount uint)
    (comments (string-utf8 200)))
  (let (
    (claim (unwrap! (map-get? impact-claims { claim-id: claim-id }) ERR-CLAIM-NOT-FOUND))
    (project-id (get project-id claim))
    (now-block-height block-height)
  )
    ;; Validate
    (asserts! (is-project-validator project-id tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-CLAIM-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none (map-get? verifications { claim-id: claim-id, validator: tx-sender })) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        approved: approved,
        verified-amount: verified-amount,
        comments: comments
      }
    )
    
    ;; Update the claim with this verification
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
      (new-verified-amount (if approved 
                              (if (> (get verified-amount claim) u0)
                                  ;; Average with existing verified amount
                                  (/ (+ (get verified-amount claim) verified-amount) u2)
                                  ;; First verification
                                  verified-amount)
                              ;; Not approved
                              (get verified-amount claim)))
    )
      (map-set impact-claims 
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received,
          verified-amount: new-verified-amount
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Register an external data source (admin only)
(define-public (register-data-source
    (source-id (string-ascii 24))
    (name (string-utf8 100))
    (description (string-utf8 200))
    (interface-principal principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
    (map-set authorized-data-sources
      { source-id: source-id }
      {
        name: name,
        description: description,
        interface-principal: interface-principal,
        authorized-at: block-height
      }
    )
    (ok true)
  )
)

;; Submit external verification (must be from registered data source)
(define-public (submit-external-verification
    (claim-id uint)
    (source-id (string-ascii 24))
    (approved bool)
    (verified-amount uint)
    (verification-data (string-utf8 500)))
  (let (
    (data-source (unwrap! (map-get? authorized-data-sources { source-id: source-id }) ERR-DATA-SOURCE-NOT-AUTHORIZED))
    (claim (unwrap! (map-get? impact-claims { claim-id: claim-id }) ERR-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Validate
    (asserts! (is-eq tx-sender (get interface-principal data-source)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status claim) "pending") ERR-CLAIM-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    
    ;; Save external verification
    (map-set external-verifications
      { claim-id: claim-id, source-id: source-id }
      {
        verified-at: now-block-height,
        approved: approved,
        verified-amount: verified-amount,
        verification-data: verification-data
      }
    )
    
    ;; Update the claim with this verification (counts as 2 regular verifications)
    (let (
      (new-verifications-received (+ (get verifications-received claim) u2))
      (new-verified-amount (if approved 
                             (if (> (get verified-amount claim) u0)
                                 ;; Average with existing verified amount but give more weight to external verification
                                 (/ (+ (+ (get verified-amount claim) verified-amount) verified-amount) u3)
                                 ;; First verification
                                 verified-amount)
                             ;; Not approved
                             (get verified-amount claim)))
    )
      (map-set impact-claims 
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received,
          verified-amount: new-verified-amount
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Finalize claim with expired verification period
(define-public (finalize-expired-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? impact-claims { claim-id: claim-id }) ERR-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u114))
    (asserts! (is-eq (get status claim) "pending") ERR-CLAIM-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-claim-status claim-id)
          (ok true))
        (begin
          (map-set impact-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)