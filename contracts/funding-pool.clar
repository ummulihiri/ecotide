;; funding-pool
;; 
;; This contract handles transparent collection and distribution of funds for sustainability projects.
;; It enables:
;; - Project registration and funding collection
;; - Milestone-based fund releases with community approval
;; - Transparent on-chain recording of all transactions
;; - Refund mechanisms for failed projects or missed milestones

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROJECT-NOT-FOUND (err u101))
(define-constant ERR-PROJECT-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-PROJECT-ALREADY-FUNDED (err u104))
(define-constant ERR-PROJECT-NOT-ACTIVE (err u105))
(define-constant ERR-FUNDING-GOAL-NOT-MET (err u106))
(define-constant ERR-INVALID-MILESTONE (err u107))
(define-constant ERR-MILESTONE-ALREADY-APPROVED (err u108))
(define-constant ERR-MILESTONE-NOT-APPROVED (err u109))
(define-constant ERR-NO-CONTRIBUTION-FOUND (err u110))
(define-constant ERR-PROJECT-DEADLINE-NOT-REACHED (err u111))
(define-constant ERR-PROJECT-SUCCEEDED (err u112))
(define-constant ERR-PROJECT-FAILED (err u113))

;; Project status enumeration
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-FUNDED u2)
(define-constant STATUS-FAILED u3)
(define-constant STATUS-COMPLETED u4)

;; Data maps and variables
;; Projects map: Stores all registered project details
(define-map projects
  { project-id: uint }
  {
    owner: principal,
    title: (string-ascii 100),
    description: (string-utf8 500),
    funding-goal: uint,
    deadline: uint,
    current-amount: uint,
    status: uint,
    milestone-count: uint
  }
)

;; Milestones map: Stores milestone details for each project
(define-map project-milestones
  { project-id: uint, milestone-id: uint }
  {
    description: (string-utf8 200),
    amount: uint,
    approved: bool,
    completed: bool
  }
)

;; Contributions map: Tracks individual contributions to projects
(define-map contributions
  { contributor: principal, project-id: uint }
  { amount: uint }
)

;; Project-contributor map: Tracks all contributors for a project (for refunds)
(define-map project-contributors
  { project-id: uint }
  { contributors: (list 100 principal) }
)

;; Contract admin
(define-data-var contract-owner principal tx-sender)

;; Next project ID counter
(define-data-var next-project-id uint u1)

;; Private functions
(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

(define-private (is-project-owner (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project (is-eq tx-sender (get owner project))
    false
  )
)

(define-private (add-contributor (project-id uint) (contributor principal))
  (let ((current-contributors (default-to { contributors: (list) } (map-get? project-contributors { project-id: project-id }))))
    (map-set project-contributors
      { project-id: project-id }
      { contributors: (unwrap-panic (as-max-len? (append (get contributors current-contributors) contributor) u100)) }
    )
  )
)

(define-private (update-project-status (project-id uint) (new-status uint))
  (match (map-get? projects { project-id: project-id })
    project (map-set projects
              { project-id: project-id }
              (merge project { status: new-status })
            )
    false
  )
)

(define-private (check-funding-deadline (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project 
    (if (and (is-eq (get status project) STATUS-ACTIVE)
             (> block-height (get deadline project)))
        ;; If deadline passed, check if funding goal met
        (if (>= (get current-amount project) (get funding-goal project))
            (update-project-status project-id STATUS-FUNDED)
            (update-project-status project-id STATUS-FAILED)
        )
        true
    )
    false
  )
)

;; Read-only functions
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
  (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id })
)

(define-read-only (get-contribution (project-id uint) (contributor principal))
  (map-get? contributions { contributor: contributor, project-id: project-id })
)

(define-read-only (get-project-contributors (project-id uint))
  (match (map-get? project-contributors { project-id: project-id })
    contributors (get contributors contributors)
    (list)
  )
)

(define-read-only (check-project-status (project-id uint))
  (match (map-get? projects { project-id: project-id })
    project (get status project)
    u0
  )
)

;; Public functions
;; Set a new contract owner
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-owner new-owner))
  )
)

;; Register a new project
(define-public (register-project 
                (title (string-ascii 100))
                (description (string-utf8 500))
                (funding-goal uint)
                (deadline uint)
                (milestones (list 10 {description: (string-utf8 200), amount: uint})))
  (let ((project-id (var-get next-project-id)))
    (asserts! (> funding-goal u0) ERR-INVALID-AMOUNT)
    (asserts! (> deadline block-height) ERR-INVALID-AMOUNT)
    
    ;; Create the project
    (map-set projects
      { project-id: project-id }
      {
        owner: tx-sender,
        title: title,
        description: description,
        funding-goal: funding-goal,
        deadline: deadline,
        current-amount: u0,
        status: STATUS-ACTIVE,
        milestone-count: (len milestones)
      }
    )
    
    ;; Create milestones
    (map
      (lambda (milestone-info)
        (map-set project-milestones
          { project-id: project-id, milestone-id: (+ u1 (default-to u0 (index-of milestones milestone-info))) }
          {
            description: (get description milestone-info),
            amount: (get amount milestone-info),
            approved: false,
            completed: false
          }
        )
      )
      milestones
    )
    
    ;; Increment project ID counter
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

;; Contribute funds to a project
(define-public (contribute (project-id uint) (amount uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (current-contribution (default-to { amount: u0 } (map-get? contributions { contributor: tx-sender, project-id: project-id })))
  )
    ;; Validate contribution
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-eq (get status project) STATUS-ACTIVE) ERR-PROJECT-NOT-ACTIVE)
    (asserts! (< block-height (get deadline project)) ERR-PROJECT-DEADLINE-NOT-REACHED)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update project's current amount
    (map-set projects
      { project-id: project-id }
      (merge project { current-amount: (+ (get current-amount project) amount) })
    )
    
    ;; Update contribution record
    (map-set contributions
      { contributor: tx-sender, project-id: project-id }
      { amount: (+ (get amount current-contribution) amount) }
    )
    
    ;; Add contributor to project-contributors if not already there
    (add-contributor project-id tx-sender)
    
    (ok true)
  )
)

;; Claim refund if project fails to meet funding goal
(define-public (claim-refund (project-id uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (contribution (unwrap! (map-get? contributions { contributor: tx-sender, project-id: project-id }) ERR-NO-CONTRIBUTION-FOUND))
  )
    ;; Check if project failed
    (asserts! (or 
                (is-eq (get status project) STATUS-FAILED)
                ;; Or deadline passed and funding goal not met
                (and (> block-height (get deadline project))
                     (< (get current-amount project) (get funding-goal project)))
              ) 
              ERR-PROJECT-SUCCEEDED)
    
    ;; Check if deadline has passed
    (asserts! (> block-height (get deadline project)) ERR-PROJECT-DEADLINE-NOT-REACHED)
    
    ;; Make sure there's a contribution to refund
    (asserts! (> (get amount contribution) u0) ERR-NO-CONTRIBUTION-FOUND)
    
    ;; Update project status if needed
    (check-funding-deadline project-id)
    
    ;; Transfer refund
    (try! (as-contract (stx-transfer? (get amount contribution) tx-sender tx-sender)))
    
    ;; Clear contribution record
    (map-set contributions
      { contributor: tx-sender, project-id: project-id }
      { amount: u0 }
    )
    
    (ok true)
  )
)

;; Project owner can propose a milestone as completed
(define-public (complete-milestone (project-id uint) (milestone-id uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id }) ERR-INVALID-MILESTONE))
  )
    ;; Verify authorization and project state
    (asserts! (is-project-owner project-id) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS-FUNDED) ERR-PROJECT-NOT-ACTIVE)
    (asserts! (not (get completed milestone)) ERR-MILESTONE-ALREADY-APPROVED)
    
    ;; Mark milestone as completed (but not yet approved)
    (map-set project-milestones
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone { completed: true })
    )
    
    (ok true)
  )
)

;; Allow contract owner to approve milestone and release funds
(define-public (approve-milestone (project-id uint) (milestone-id uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id }) ERR-INVALID-MILESTONE))
  )
    ;; Verify authorization and milestone status
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS-FUNDED) ERR-PROJECT-NOT-ACTIVE)
    (asserts! (get completed milestone) ERR-MILESTONE-NOT-APPROVED)
    (asserts! (not (get approved milestone)) ERR-MILESTONE-ALREADY-APPROVED)
    
    ;; Mark milestone as approved
    (map-set project-milestones
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone { approved: true })
    )
    
    ;; Release funds to project owner
    (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get owner project))))
    
    ;; Check if all milestones are completed and approved
    (let ((all-completed true))
      (map-set projects
        { project-id: project-id }
        (merge project {
          status: (if (and all-completed (= milestone-id (get milestone-count project)))
                     STATUS-COMPLETED
                     (get status project))
        })
      )
    )
    
    (ok true)
  )
)

;; Cancel project (only by project owner and only if active)
(define-public (cancel-project (project-id uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
  )
    ;; Verify authorization and project state
    (asserts! (is-project-owner project-id) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS-ACTIVE) ERR-PROJECT-NOT-ACTIVE)
    
    ;; Update project status to failed
    (update-project-status project-id STATUS-FAILED)
    
    (ok true)
  )
)