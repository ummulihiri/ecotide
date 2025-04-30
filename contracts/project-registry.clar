;; project-registry
;; 
;; This is the central registry for all sustainable development projects on the EcoTide platform.
;; It allows community members to create, update, and discover sustainable development projects
;; with a focus on environmental initiatives such as reforestation, clean water, and more.
;; Projects are tracked from proposal through completion, with metadata about their goals,
;; impact metrics, and status.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROJECT-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATUS (err u102))
(define-constant ERR-INVALID-PARAMETER (err u103))
(define-constant ERR-PROJECT-EXISTS (err u104))
(define-constant ERR-STATUS-TRANSITION-INVALID (err u105))

;; Project status types
(define-constant STATUS-PROPOSED u1)
(define-constant STATUS-ACTIVE u2)
(define-constant STATUS-COMPLETED u3)
(define-constant STATUS-CANCELLED u4)

;; Environmental category types
(define-constant CATEGORY-REFORESTATION u1)
(define-constant CATEGORY-CLEAN-WATER u2)
(define-constant CATEGORY-RENEWABLE-ENERGY u3)
(define-constant CATEGORY-WASTE-MANAGEMENT u4)
(define-constant CATEGORY-BIODIVERSITY u5)
(define-constant CATEGORY-EDUCATION u6)
(define-constant CATEGORY-OTHER u99)

;; Data maps and variables

;; Counter for project IDs
(define-data-var next-project-id uint u1)

;; Core project data
(define-map projects
  { project-id: uint }
  {
    creator: principal,
    name: (string-ascii 100),
    description: (string-utf8 500),
    category: uint,
    location: (string-ascii 100),
    status: uint,
    created-at: uint,
    updated-at: uint
  }
)

;; Project funding details
(define-map project-funding
  { project-id: uint }
  {
    funding-goal: uint,
    funding-received: uint,
    funding-deadline: uint,
    funds-released: bool
  }
)

;; Project impact metrics
(define-map project-impact
  { project-id: uint }
  {
    projected-impact: (string-utf8 500),
    actual-impact: (string-utf8 500),
    impact-metrics: (list 10 (string-utf8 100))
  }
)

;; Project-to-owner mapping for additional authorized collaborators
(define-map project-collaborators
  { project-id: uint, collaborator: principal }
  { can-update: bool, can-withdraw: bool }
)

;; Projects by category for discovery
(define-map projects-by-category
  { category: uint }
  { project-ids: (list 1000 uint) }
)

;; Projects by location for discovery
(define-map projects-by-location
  { location: (string-ascii 100) }
  { project-ids: (list 1000 uint) }
)

;; Projects by status for filtering
(define-map projects-by-status
  { status: uint }
  { project-ids: (list 1000 uint) }
)

;; Private functions

;; Check if caller is authorized to modify the project
(define-private (is-authorized (project-id uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) false))
  )
    (or
      (is-eq tx-sender (get creator project))
      (default-to false 
        (get can-update 
          (default-to { can-update: false, can-withdraw: false }
            (map-get? project-collaborators { project-id: project-id, collaborator: tx-sender })
          )
        )
      )
    )
  )
)

;; Check if a status transition is valid
(define-private (is-valid-status-transition (current-status uint) (new-status uint))
  (or
    ;; Proposed -> Active or Cancelled
    (and (is-eq current-status STATUS-PROPOSED) 
         (or (is-eq new-status STATUS-ACTIVE) 
             (is-eq new-status STATUS-CANCELLED)))
    
    ;; Active -> Completed or Cancelled
    (and (is-eq current-status STATUS-ACTIVE) 
         (or (is-eq new-status STATUS-COMPLETED) 
             (is-eq new-status STATUS-CANCELLED)))
    
    ;; No transitions from completed or cancelled
    (and (or (is-eq current-status STATUS-COMPLETED) 
             (is-eq current-status STATUS-CANCELLED))
         (is-eq new-status current-status))
  )
)

;; Add a project ID to a category list
(define-private (add-to-category (project-id uint) (category uint))
  (let (
    (current-list (default-to { project-ids: (list) } 
                    (map-get? projects-by-category { category: category })))
  )
    (map-set projects-by-category 
      { category: category }
      { project-ids: (unwrap! (as-max-len? 
                                (append (get project-ids current-list) project-id)
                                u1000)
                              ERR-INVALID-PARAMETER) }
    )
  )
)

;; Add a project ID to a location list
(define-private (add-to-location (project-id uint) (location (string-ascii 100)))
  (let (
    (current-list (default-to { project-ids: (list) } 
                    (map-get? projects-by-location { location: location })))
  )
    (map-set projects-by-location 
      { location: location }
      { project-ids: (unwrap! (as-max-len? 
                                (append (get project-ids current-list) project-id)
                                u1000)
                              ERR-INVALID-PARAMETER) }
    )
  )
)

;; Add a project ID to a status list
(define-private (add-to-status (project-id uint) (status uint))
  (let (
    (current-list (default-to { project-ids: (list) } 
                    (map-get? projects-by-status { status: status })))
  )
    (map-set projects-by-status 
      { status: status }
      { project-ids: (unwrap! (as-max-len? 
                                (append (get project-ids current-list) project-id)
                                u1000)
                              ERR-INVALID-PARAMETER) }
    )
  )
)

;; Remove a project ID from a status list
(define-private (remove-from-status (project-id uint) (status uint))
  (let (
    (current-list (default-to { project-ids: (list) } 
                    (map-get? projects-by-status { status: status })))
    (filtered-list (filter (lambda (id) (not (is-eq id project-id))) 
                          (get project-ids current-list)))
  )
    (map-set projects-by-status 
      { status: status }
      { project-ids: filtered-list }
    )
  )
)

;; Get the current block height (for timestamps)
(define-private (get-block-height)
  block-height
)

;; Public functions

;; Create a new project
(define-public (create-project 
  (name (string-ascii 100))
  (description (string-utf8 500))
  (category uint)
  (location (string-ascii 100))
  (funding-goal uint)
  (funding-deadline uint)
  (projected-impact (string-utf8 500))
  (impact-metrics (list 10 (string-utf8 100)))
)
  (let (
    (new-project-id (var-get next-project-id))
    (block-time (get-block-height))
  )
    ;; Validate parameters
    (asserts! (> (len name) u0) ERR-INVALID-PARAMETER)
    (asserts! (> (len description) u0) ERR-INVALID-PARAMETER)
    (asserts! (and (>= category u1) (<= category u99)) ERR-INVALID-PARAMETER)
    (asserts! (> (len location) u0) ERR-INVALID-PARAMETER)
    (asserts! (> funding-goal u0) ERR-INVALID-PARAMETER)
    (asserts! (> funding-deadline block-time) ERR-INVALID-PARAMETER)
    
    ;; Store core project data
    (map-set projects
      { project-id: new-project-id }
      {
        creator: tx-sender,
        name: name,
        description: description,
        category: category,
        location: location,
        status: STATUS-PROPOSED,
        created-at: block-time,
        updated-at: block-time
      }
    )
    
    ;; Store funding details
    (map-set project-funding
      { project-id: new-project-id }
      {
        funding-goal: funding-goal,
        funding-received: u0,
        funding-deadline: funding-deadline,
        funds-released: false
      }
    )
    
    ;; Store impact metrics
    (map-set project-impact
      { project-id: new-project-id }
      {
        projected-impact: projected-impact,
        actual-impact: "",
        impact-metrics: impact-metrics
      }
    )
    
    ;; Update indexes for discovery
    (try! (add-to-category new-project-id category))
    (try! (add-to-location new-project-id location))
    (try! (add-to-status new-project-id STATUS-PROPOSED))
    
    ;; Increment the project ID counter
    (var-set next-project-id (+ new-project-id u1))
    
    ;; Return success with the new project ID
    (ok new-project-id)
  )
)

;; Update an existing project's basic information
(define-public (update-project
  (project-id uint)
  (name (string-ascii 100))
  (description (string-utf8 500))
  (location (string-ascii 100))
)
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (block-time (get-block-height))
  )
    ;; Check authorization
    (asserts! (is-authorized project-id) ERR-NOT-AUTHORIZED)
    
    ;; Validate parameters
    (asserts! (> (len name) u0) ERR-INVALID-PARAMETER)
    (asserts! (> (len description) u0) ERR-INVALID-PARAMETER)
    (asserts! (> (len location) u0) ERR-INVALID-PARAMETER)
    
    ;; Check if location is changing and update index if needed
    (if (not (is-eq location (get location project)))
      (begin
        (try! (add-to-location project-id location))
        ;; Note: We're not removing from the old location to preserve search history
      )
      true
    )
    
    ;; Update the project data
    (map-set projects
      { project-id: project-id }
      (merge project 
        {
          name: name,
          description: description,
          location: location,
          updated-at: block-time
        }
      )
    )
    
    (ok true)
  )
)

;; Update a project's status
(define-public (update-project-status
  (project-id uint)
  (new-status uint)
)
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (current-status (get status project))
    (block-time (get-block-height))
  )
    ;; Check authorization
    (asserts! (is-authorized project-id) ERR-NOT-AUTHORIZED)
    
    ;; Validate status code
    (asserts! (and (>= new-status STATUS-PROPOSED) 
                   (<= new-status STATUS-CANCELLED)) 
              ERR-INVALID-STATUS)
    
    ;; Verify valid status transition
    (asserts! (is-valid-status-transition current-status new-status) 
              ERR-STATUS-TRANSITION-INVALID)
    
    ;; Update the index for filtering
    (try! (remove-from-status project-id current-status))
    (try! (add-to-status project-id new-status))
    
    ;; Update the project data
    (map-set projects
      { project-id: project-id }
      (merge project 
        {
          status: new-status,
          updated-at: block-time
        }
      )
    )
    
    (ok true)
  )
)

;; Update a project's impact information
(define-public (update-project-impact
  (project-id uint)
  (actual-impact (string-utf8 500))
)
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (impact-data (unwrap! (map-get? project-impact { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (block-time (get-block-height))
  )
    ;; Check authorization
    (asserts! (is-authorized project-id) ERR-NOT-AUTHORIZED)
    
    ;; Validate parameters
    (asserts! (> (len actual-impact) u0) ERR-INVALID-PARAMETER)
    
    ;; Update the impact data
    (map-set project-impact
      { project-id: project-id }
      (merge impact-data
        {
          actual-impact: actual-impact
        }
      )
    )
    
    ;; Update the project's updated-at timestamp
    (map-set projects
      { project-id: project-id }
      (merge project 
        {
          updated-at: block-time
        }
      )
    )
    
    (ok true)
  )
)

;; Add a collaborator to the project
(define-public (add-collaborator
  (project-id uint)
  (collaborator principal)
  (can-update bool)
  (can-withdraw bool)
)
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
  )
    ;; Only the creator can add collaborators
    (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
    
    ;; Can't add yourself as a collaborator
    (asserts! (not (is-eq collaborator (get creator project))) ERR-INVALID-PARAMETER)
    
    ;; Add the collaborator
    (map-set project-collaborators
      { project-id: project-id, collaborator: collaborator }
      { can-update: can-update, can-withdraw: can-withdraw }
    )
    
    (ok true)
  )
)

;; Remove a collaborator from the project
(define-public (remove-collaborator
  (project-id uint)
  (collaborator principal)
)
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
  )
    ;; Only the creator can remove collaborators
    (asserts! (is-eq tx-sender (get creator project)) ERR-NOT-AUTHORIZED)
    
    ;; Delete the collaborator entry
    (map-delete project-collaborators
      { project-id: project-id, collaborator: collaborator }
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Get basic project information
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

;; Get project funding information
(define-read-only (get-project-funding (project-id uint))
  (map-get? project-funding { project-id: project-id })
)

;; Get project impact information
(define-read-only (get-project-impact (project-id uint))
  (map-get? project-impact { project-id: project-id })
)

;; Check if a user is authorized to modify a project
(define-read-only (check-authorization (project-id uint) (user principal))
  (let (
    (project (map-get? projects { project-id: project-id }))
  )
    (if (is-some project)
      (or
        (is-eq user (get creator (unwrap-panic project)))
        (is-some (map-get? project-collaborators { project-id: project-id, collaborator: user }))
      )
      false
    )
  )
)

;; Get all projects for a specific category
(define-read-only (get-projects-by-category (category uint))
  (let (
    (projects-list (map-get? projects-by-category { category: category }))
  )
    (if (is-some projects-list)
      (get project-ids (unwrap-panic projects-list))
      (list)
    )
  )
)

;; Get all projects for a specific location
(define-read-only (get-projects-by-location (location (string-ascii 100)))
  (let (
    (projects-list (map-get? projects-by-location { location: location }))
  )
    (if (is-some projects-list)
      (get project-ids (unwrap-panic projects-list))
      (list)
    )
  )
)

;; Get all projects with a specific status
(define-read-only (get-projects-by-status (status uint))
  (let (
    (projects-list (map-get? projects-by-status { status: status }))
  )
    (if (is-some projects-list)
      (get project-ids (unwrap-panic projects-list))
      (list)
    )
  )
)

;; Get the total number of projects in the system
(define-read-only (get-project-count)
  (- (var-get next-project-id) u1)
)