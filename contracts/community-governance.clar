;; community-governance.clar
;; A governance contract that facilitates democratic decision-making for local sustainability projects.
;; This contract allows community stakeholders to propose, vote on, and execute decisions
;; related to project approvals, milestone validations, and fund disbursements.

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-EXISTS (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-VOTING-CLOSED (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-PROPOSAL-NOT-ACTIVE (err u105))
(define-constant ERR-PROPOSAL-ACTIVE (err u106))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u107))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u108))
(define-constant ERR-INVALID-VOTE (err u109))
(define-constant ERR-INVALID-PROPOSAL-TYPE (err u110))
(define-constant ERR-VOTING-PERIOD-NOT-ENDED (err u111))

;; Constants
(define-constant PROPOSAL-TYPE-PROJECT u1)
(define-constant PROPOSAL-TYPE-MILESTONE u2)
(define-constant PROPOSAL-TYPE-DISBURSEMENT u3)

(define-constant VOTE-FOR true)
(define-constant VOTE-AGAINST false)

(define-constant MINIMUM-VOTING-POWER u10) ;; Minimum voting power required to create a proposal

;; Data Maps and Variables

;; Tracks each user's voting power in the community governance
(define-map user-voting-power principal uint)

;; Proposal information
(define-map proposals
  { proposal-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-utf8 1000),
    proposal-type: uint,  ;; 1 = Project Approval, 2 = Milestone Validation, 3 = Fund Disbursement
    target-contract: principal, ;; Contract to call if proposal passes
    target-function: (string-ascii 128), ;; Function to call if proposal passes
    target-arguments: (list 10 (optional (string-utf8 256))), ;; Arguments to pass to function
    votes-for: uint,
    votes-against: uint,
    status: uint, ;; 0 = Active, 1 = Passed, 2 = Failed, 3 = Executed
    start-block-height: uint,
    end-block-height: uint,
    execution-block-height: (optional uint)
  }
)

;; Tracks votes for each proposal
(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voting-power: uint }
)

;; Counter for proposal IDs
(define-data-var proposal-id-counter uint u0)

;; Functions

;; Get the current proposal ID counter
(define-read-only (get-proposal-id-counter)
  (var-get proposal-id-counter)
)

;; Get a user's voting power
(define-read-only (get-voting-power (user principal))
  (default-to u0 (map-get? user-voting-power user))
)

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Get a user's vote on a proposal
(define-read-only (get-user-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

;; Check if a proposal exists
(define-read-only (proposal-exists? (proposal-id uint))
  (is-some (map-get? proposals { proposal-id: proposal-id }))
)

;; Check if a proposal is active
(define-read-only (is-proposal-active? (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (and 
               (is-eq (get status proposal) u0)
               (>= block-height (get start-block-height proposal))
               (<= block-height (get end-block-height proposal)))
    false
  )
)

;; Check if voting has ended for a proposal
(define-read-only (has-voting-ended? (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (> block-height (get end-block-height proposal))
    false
  )
)

;; Check if a proposal can be executed
(define-read-only (can-execute-proposal? (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (and 
               (is-eq (get status proposal) u1) ;; Passed
               (is-none (get execution-block-height proposal)))
    false
  )
)

;; Calculate proposal result after voting period
(define-read-only (calculate-proposal-result (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (if (> (get votes-for proposal) (get votes-against proposal)) u1 u2) ;; 1 = Passed, 2 = Failed
    u2
  )
)

;; Update a user's voting power
;; This would typically be called by another contract (e.g., when a user contributes to a project)
(define-public (update-voting-power (user principal) (amount uint))
  (begin
    (asserts! (or (is-eq tx-sender contract-caller) (is-eq tx-sender user)) ERR-NOT-AUTHORIZED)
    
    (map-set user-voting-power
      user
      (+ (default-to u0 (map-get? user-voting-power user)) amount)
    )
    (ok true)
  )
)

;; Create a new proposal
(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-utf8 1000))
  (proposal-type uint)
  (target-contract principal)
  (target-function (string-ascii 128))
  (target-arguments (list 10 (optional (string-utf8 256))))
  (voting-period-length uint)
)
  (let
    (
      (new-proposal-id (+ (var-get proposal-id-counter) u1))
      (creator-voting-power (default-to u0 (map-get? user-voting-power tx-sender)))
      (start-block (block-height))
      (end-block (+ block-height voting-period-length))
    )
    
    ;; Validate the proposal
    (asserts! (>= creator-voting-power MINIMUM-VOTING-POWER) ERR-INSUFFICIENT-VOTING-POWER)
    (asserts! (or (is-eq proposal-type PROPOSAL-TYPE-PROJECT)
                 (is-eq proposal-type PROPOSAL-TYPE-MILESTONE)
                 (is-eq proposal-type PROPOSAL-TYPE-DISBURSEMENT)) ERR-INVALID-PROPOSAL-TYPE)
    
    ;; Create the proposal
    (map-set proposals
      { proposal-id: new-proposal-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        proposal-type: proposal-type,
        target-contract: target-contract,
        target-function: target-function,
        target-arguments: target-arguments,
        votes-for: u0,
        votes-against: u0,
        status: u0, ;; Active
        start-block-height: start-block,
        end-block-height: end-block,
        execution-block-height: none
      }
    )
    
    ;; Update the proposal counter
    (var-set proposal-id-counter new-proposal-id)
    
    (ok new-proposal-id)
  )
)

;; Cast a vote on a proposal
(define-public (vote (proposal-id uint) (vote-for bool))
  (let
    (
      (voter tx-sender)
      (voting-power (default-to u0 (map-get? user-voting-power voter)))
    )
    
    ;; Validate the vote
    (asserts! (proposal-exists? proposal-id) ERR-PROPOSAL-NOT-FOUND)
    (asserts! (is-proposal-active? proposal-id) ERR-PROPOSAL-NOT-ACTIVE)
    (asserts! (is-none (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })) ERR-ALREADY-VOTED)
    (asserts! (> voting-power u0) ERR-INSUFFICIENT-VOTING-POWER)
    
    ;; Record the vote
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: voter }
      { vote: vote-for, voting-power: voting-power }
    )
    
    ;; Update vote tallies in the proposal
    (match (map-get? proposals { proposal-id: proposal-id })
      proposal 
        (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal {
            votes-for: (if vote-for (+ (get votes-for proposal) voting-power) (get votes-for proposal)),
            votes-against: (if vote-for (get votes-against proposal) (+ (get votes-against proposal) voting-power))
          })
        )
      ERR-PROPOSAL-NOT-FOUND
    )
    
    (ok true)
  )
)

;; Finalize a proposal after voting has ended
(define-public (finalize-proposal (proposal-id uint))
  (let
    (
      (result none)
    )
    ;; Validate the proposal state
    (asserts! (proposal-exists? proposal-id) ERR-PROPOSAL-NOT-FOUND)
    (asserts! (has-voting-ended? proposal-id) ERR-PROPOSAL-ACTIVE)
    
    (match (map-get? proposals { proposal-id: proposal-id })
      proposal 
        (begin
          ;; Only finalize if it's still in active state
          (asserts! (is-eq (get status proposal) u0) ERR-PROPOSAL-NOT-ACTIVE)
          
          ;; Calculate the result
          (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { status: (calculate-proposal-result proposal-id) })
          )
          (ok true)
        )
      ERR-PROPOSAL-NOT-FOUND
    )
  )
)

;; Execute a passed proposal
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (result none)
    )
    ;; Validate the proposal state
    (asserts! (proposal-exists? proposal-id) ERR-PROPOSAL-NOT-FOUND)
    (asserts! (can-execute-proposal? proposal-id) ERR-PROPOSAL-NOT-ACTIVE)
    
    (match (map-get? proposals { proposal-id: proposal-id })
      proposal
        (begin
          ;; Mark the proposal as executed
          (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { 
              status: u3, ;; Executed
              execution-block-height: (some block-height)
            })
          )
          
          ;; TODO: In a production environment, this would invoke the target contract and function
          ;; This is a placeholder for that functionality
          ;; The actual implementation would involve contract calls using the target-contract,
          ;; target-function, and target-arguments fields
          
          (ok true)
        )
      ERR-PROPOSAL-NOT-FOUND
    )
  )
)