;; brightlink-connections
;; 
;; This contract manages professional connections between users on the BrightLink platform.
;; It allows users to establish, manage, and organize their professional network with
;; customized relationship types, tags, and privacy settings.
;;
;; The contract implements a full connection lifecycle:
;; 1. Connection requests (pending state)
;; 2. Acceptance/rejection workflow
;; 3. Active connection management including relationship types and tags
;; 4. Privacy controls for network visibility
;; 5. Archive functionality for inactive connections

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-USER-NOT-FOUND (err u1001))
(define-constant ERR-ALREADY-CONNECTED (err u1002))
(define-constant ERR-CONNECTION-NOT-FOUND (err u1003))
(define-constant ERR-INVALID-STATE (err u1004))
(define-constant ERR-INVALID-RELATIONSHIP-TYPE (err u1005))
(define-constant ERR-INVALID-VISIBILITY (err u1006))
(define-constant ERR-SELF-CONNECTION (err u1007))
(define-constant ERR-REQUEST-ALREADY-PENDING (err u1008))
(define-constant ERR-TAG-LIMIT-EXCEEDED (err u1009))
(define-constant ERR-INVALID-TAG (err u1010))

;; Connection states
(define-constant STATE-PENDING "pending")
(define-constant STATE-CONNECTED "connected")
(define-constant STATE-ARCHIVED "archived")
(define-constant STATE-REJECTED "rejected")

;; Relationship types
(define-constant RELATIONSHIP-COLLEAGUE "colleague")
(define-constant RELATIONSHIP-MENTOR "mentor")
(define-constant RELATIONSHIP-MENTEE "mentee")
(define-constant RELATIONSHIP-CLIENT "client")
(define-constant RELATIONSHIP-SERVICE-PROVIDER "service-provider")
(define-constant RELATIONSHIP-OTHER "other")

;; Visibility options
(define-constant VISIBILITY-PUBLIC "public")
(define-constant VISIBILITY-CONNECTIONS-ONLY "connections-only")
(define-constant VISIBILITY-PRIVATE "private")

;; Max allowed tags per connection
(define-constant MAX-TAGS-PER-CONNECTION u5)

;; Data structures

;; Stores the connection between two users
(define-map connections 
  { user: principal, connection-with: principal }
  {
    state: (string-ascii 20),
    relationship-type: (string-ascii 30),
    visibility: (string-ascii 20),
    tags: (list 5 (string-ascii 30)),
    notes: (optional (string-utf8 500)),
    created-at: uint,
    updated-at: uint
  }
)

;; Stores a user's connections for easy lookup
(define-map user-connections
  { user: principal }
  { connections: (list 500 principal) }
)

;; Stores a user's pending connection requests
(define-map pending-requests
  { user: principal }
  { requests: (list 500 principal) }
)

;; Private functions

;; Check if a relationship type is valid
(define-private (is-valid-relationship-type (relationship-type (string-ascii 30)))
  (or
    (is-eq relationship-type RELATIONSHIP-COLLEAGUE)
    (is-eq relationship-type RELATIONSHIP-MENTOR)
    (is-eq relationship-type RELATIONSHIP-MENTEE)
    (is-eq relationship-type RELATIONSHIP-CLIENT)
    (is-eq relationship-type RELATIONSHIP-SERVICE-PROVIDER)
    (is-eq relationship-type RELATIONSHIP-OTHER)
  )
)

;; Check if visibility setting is valid
(define-private (is-valid-visibility (visibility (string-ascii 20)))
  (or
    (is-eq visibility VISIBILITY-PUBLIC)
    (is-eq visibility VISIBILITY-CONNECTIONS-ONLY)
    (is-eq visibility VISIBILITY-PRIVATE)
  )
)

;; Check if a connection exists between two users
(define-private (connection-exists (user-a principal) (user-b principal))
  (is-some (map-get? connections { user: user-a, connection-with: user-b }))
)

;; Check if two users are connected with "connected" state
(define-private (are-connected (user-a principal) (user-b principal))
  (match (map-get? connections { user: user-a, connection-with: user-b })
    connection (is-eq (get state connection) STATE-CONNECTED)
    false
  )
)

;; Get connection details
(define-private (get-connection (user principal) (connection-with principal))
  (map-get? connections { user: user, connection-with: connection-with })
)

;; Add a principal to a list if it doesn't already exist
(define-private (add-to-list (existing-list (list 500 principal)) (new-item principal))
  (if (is-some (index-of existing-list new-item))
    existing-list
    (unwrap-panic (as-max-len? (append existing-list new-item) u500))
  )
)

;; Remove a principal from a list
(define-private (remove-from-list (existing-list (list 500 principal)) (item-to-remove principal))
  (filter (lambda (item) (not (is-eq item item-to-remove))) existing-list)
)

;; Add a new connection to user's connection list
(define-private (add-to-connections (user principal) (connection-with principal))
  (let (
    (current-connections (default-to { connections: (list) } (map-get? user-connections { user: user })))
  )
    (map-set user-connections
      { user: user }
      { connections: (add-to-list (get connections current-connections) connection-with) }
    )
  )
)

;; Add a pending request to user's request list
(define-private (add-to-pending-requests (user principal) (requester principal))
  (let (
    (current-requests (default-to { requests: (list) } (map-get? pending-requests { user: user })))
  )
    (map-set pending-requests
      { user: user }
      { requests: (add-to-list (get requests current-requests) requester) }
    )
  )
)

;; Remove a pending request from user's request list
(define-private (remove-from-pending-requests (user principal) (requester principal))
  (let (
    (current-requests (default-to { requests: (list) } (map-get? pending-requests { user: user })))
  )
    (map-set pending-requests
      { user: user }
      { requests: (remove-from-list (get requests current-requests) requester) }
    )
  )
)

;; Check if a connection request is pending
(define-private (is-request-pending (from principal) (to principal))
  (match (map-get? connections { user: from, connection-with: to })
    connection (is-eq (get state connection) STATE-PENDING)
    false
  )
)

;; Create a new bidirectional connection between users
(define-private (create-bidirectional-connection
  (user-a principal)
  (user-b principal)
  (state (string-ascii 20))
  (relationship-type-a (string-ascii 30))
  (relationship-type-b (string-ascii 30))
)
  (let (
    (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
  )
    ;; Set connection from user-a to user-b
    (map-set connections
      { user: user-a, connection-with: user-b }
      {
        state: state,
        relationship-type: relationship-type-a,
        visibility: VISIBILITY-PUBLIC, ;; Default visibility
        tags: (list),
        notes: none,
        created-at: timestamp,
        updated-at: timestamp
      }
    )
    
    ;; Set connection from user-b to user-a
    (map-set connections
      { user: user-b, connection-with: user-a }
      {
        state: state,
        relationship-type: relationship-type-b,
        visibility: VISIBILITY-PUBLIC, ;; Default visibility
        tags: (list),
        notes: none,
        created-at: timestamp,
        updated-at: timestamp
      }
    )
    
    ;; If the state is "connected", add to both users' connection lists
    (if (is-eq state STATE-CONNECTED)
      (begin
        (add-to-connections user-a user-b)
        (add-to-connections user-b user-a)
        (ok true)
      )
      (ok true)
    )
  )
)

;; Convert a pending connection to connected
(define-private (convert-to-connected (user principal) (connection-with principal))
  (let (
    (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
    (user-connection (unwrap! (get-connection user connection-with) ERR-CONNECTION-NOT-FOUND))
    (other-connection (unwrap! (get-connection connection-with user) ERR-CONNECTION-NOT-FOUND))
  )
    ;; Update user's connection
    (map-set connections
      { user: user, connection-with: connection-with }
      (merge user-connection { state: STATE-CONNECTED, updated-at: timestamp })
    )
    
    ;; Update other's connection
    (map-set connections
      { user: connection-with, connection-with: user }
      (merge other-connection { state: STATE-CONNECTED, updated-at: timestamp })
    )
    
    ;; Add to both users' connection lists
    (add-to-connections user connection-with)
    (add-to-connections connection-with user)
    
    ;; Remove from pending requests
    (remove-from-pending-requests user connection-with)
    
    (ok true)
  )
)

;; Read-only functions

;; Get a connection between two users
(define-read-only (get-connection-details (user principal) (connection-with principal))
  (let (
    (connection (map-get? connections { user: user, connection-with: connection-with }))
  )
    (match connection
      value (ok value)
      ERR-CONNECTION-NOT-FOUND
    )
  )
)

;; Get all connections for a user with a specific state
(define-read-only (get-user-connections-by-state (user principal) (state (string-ascii 20)))
  (let (
    (user-conn-data (map-get? user-connections { user: user }))
  )
    (match user-conn-data
      data 
      (ok (filter 
        (lambda (conn-user)
          (match (map-get? connections { user: user, connection-with: conn-user })
            conn-data (is-eq (get state conn-data) state)
            false
          )
        )
        (get connections data)
      ))
      (ok (list))
    )
  )
)

;; Get all pending connection requests for a user
(define-read-only (get-pending-requests (user principal))
  (let (
    (user-requests (map-get? pending-requests { user: user }))
  )
    (match user-requests
      data (ok (get requests data))
      (ok (list))
    )
  )
)

;; Check if two users are connected (with "connected" state)
(define-read-only (check-if-connected (user-a principal) (user-b principal))
  (ok (are-connected user-a user-b))
)

;; Public functions

;; Send a connection request to another user
(define-public (send-connection-request 
  (to principal) 
  (relationship-type (string-ascii 30))
  (notes (optional (string-utf8 500)))
)
  (let (
    (sender tx-sender)
  )
    ;; Verify not connecting to self
    (asserts! (not (is-eq sender to)) ERR-SELF-CONNECTION)
    
    ;; Verify not already connected or pending
    (asserts! (not (connection-exists sender to)) ERR-ALREADY-CONNECTED)
    (asserts! (not (connection-exists to sender)) ERR-ALREADY-CONNECTED)
    
    ;; Verify relationship type is valid
    (asserts! (is-valid-relationship-type relationship-type) ERR-INVALID-RELATIONSHIP-TYPE)
    
    ;; Create a new connection in PENDING state
    (let (
      (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
    )
      ;; Create sender's connection record
      (map-set connections
        { user: sender, connection-with: to }
        {
          state: STATE-PENDING,
          relationship-type: relationship-type,
          visibility: VISIBILITY-PUBLIC,
          tags: (list),
          notes: notes,
          created-at: timestamp,
          updated-at: timestamp
        }
      )
      
      ;; Create recipient's connection record
      (map-set connections
        { user: to, connection-with: sender }
        {
          state: STATE-PENDING,
          relationship-type: RELATIONSHIP-OTHER, ;; Default until recipient updates
          visibility: VISIBILITY-PUBLIC,
          tags: (list),
          notes: none,
          created-at: timestamp,
          updated-at: timestamp
        }
      )
      
      ;; Add to recipient's pending requests
      (add-to-pending-requests to sender)
      
      (ok true)
    )
  )
)

;; Accept a connection request
(define-public (accept-connection-request 
  (from principal) 
  (relationship-type (string-ascii 30))
)
  (let (
    (recipient tx-sender)
  )
    ;; Verify request exists and is in pending state
    (asserts! (is-request-pending from recipient) ERR-CONNECTION-NOT-FOUND)
    
    ;; Verify relationship type is valid
    (asserts! (is-valid-relationship-type relationship-type) ERR-INVALID-RELATIONSHIP-TYPE)
    
    ;; Update recipient's chosen relationship type
    (let (
      (recipient-connection (unwrap! (get-connection recipient from) ERR-CONNECTION-NOT-FOUND))
      (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
    )
      (map-set connections
        { user: recipient, connection-with: from }
        (merge recipient-connection { 
          relationship-type: relationship-type,
          updated-at: timestamp
        })
      )
      
      ;; Convert both connections to CONNECTED state
      (convert-to-connected recipient from)
    )
  )
)

;; Reject a connection request
(define-public (reject-connection-request (from principal))
  (let (
    (recipient tx-sender)
    (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
  )
    ;; Verify request exists and is in pending state
    (asserts! (is-request-pending from recipient) ERR-CONNECTION-NOT-FOUND)
    
    ;; Update both connection records to REJECTED state
    (let (
      (from-connection (unwrap! (get-connection from recipient) ERR-CONNECTION-NOT-FOUND))
      (recipient-connection (unwrap! (get-connection recipient from) ERR-CONNECTION-NOT-FOUND))
    )
      ;; Update from's connection
      (map-set connections
        { user: from, connection-with: recipient }
        (merge from-connection { 
          state: STATE-REJECTED,
          updated-at: timestamp
        })
      )
      
      ;; Update recipient's connection
      (map-set connections
        { user: recipient, connection-with: from }
        (merge recipient-connection { 
          state: STATE-REJECTED,
          updated-at: timestamp
        })
      )
      
      ;; Remove from pending requests
      (remove-from-pending-requests recipient from)
      
      (ok true)
    )
  )
)

;; Update connection relationship type
(define-public (update-relationship-type 
  (connection-with principal) 
  (relationship-type (string-ascii 30))
)
  (let (
    (user tx-sender)
  )
    ;; Verify connection exists and is in CONNECTED state
    (asserts! (are-connected user connection-with) ERR-CONNECTION-NOT-FOUND)
    
    ;; Verify relationship type is valid
    (asserts! (is-valid-relationship-type relationship-type) ERR-INVALID-RELATIONSHIP-TYPE)
    
    ;; Update relationship type
    (let (
      (connection (unwrap! (get-connection user connection-with) ERR-CONNECTION-NOT-FOUND))
      (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
    )
      (map-set connections
        { user: user, connection-with: connection-with }
        (merge connection { 
          relationship-type: relationship-type,
          updated-at: timestamp
        })
      )
      
      (ok true)
    )
  )
)

;; Update connection visibility
(define-public (update-visibility 
  (connection-with principal) 
  (visibility (string-ascii 20))
)
  (let (
    (user tx-sender)
  )
    ;; Verify connection exists
    (asserts! (connection-exists user connection-with) ERR-CONNECTION-NOT-FOUND)
    
    ;; Verify visibility is valid
    (asserts! (is-valid-visibility visibility) ERR-INVALID-VISIBILITY)
    
    ;; Update visibility
    (let (
      (connection (unwrap! (get-connection user connection-with) ERR-CONNECTION-NOT-FOUND))
      (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
    )
      (map-set connections
        { user: user, connection-with: connection-with }
        (merge connection { 
          visibility: visibility,
          updated-at: timestamp
        })
      )
      
      (ok true)
    )
  )
)

;; Add a tag to a connection
(define-public (add-tag 
  (connection-with principal) 
  (tag (string-ascii 30))
)
  (let (
    (user tx-sender)
  )
    ;; Verify connection exists
    (asserts! (connection-exists user connection-with) ERR-CONNECTION-NOT-FOUND)
    
    ;; Get current connection data
    (let (
      (connection (unwrap! (get-connection user connection-with) ERR-CONNECTION-NOT-FOUND))
      (current-tags (get tags connection))
      (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
    )
      ;; Check if tag already exists or max tags reached
      (asserts! (< (len current-tags) MAX-TAGS-PER-CONNECTION) ERR-TAG-LIMIT-EXCEEDED)
      (asserts! (is-none (index-of current-tags tag)) ERR-INVALID-TAG)
      
      ;; Add new tag
      (let (
        (updated-tags (unwrap! (as-max-len? (append current-tags tag) u5) ERR-TAG-LIMIT-EXCEEDED))
      )
        (map-set connections
          { user: user, connection-with: connection-with }
          (merge connection { 
            tags: updated-tags,
            updated-at: timestamp
          })
        )
        
        (ok true)
      )
    )
  )
)

;; Remove a tag from a connection
(define-public (remove-tag 
  (connection-with principal) 
  (tag (string-ascii 30))
)
  (let (
    (user tx-sender)
  )
    ;; Verify connection exists
    (asserts! (connection-exists user connection-with) ERR-CONNECTION-NOT-FOUND)
    
    ;; Get current connection data
    (let (
      (connection (unwrap! (get-connection user connection-with) ERR-CONNECTION-NOT-FOUND))
      (current-tags (get tags connection))
      (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
    )
      ;; Filter out the tag
      (let (
        (updated-tags (filter (lambda (t) (not (is-eq t tag))) current-tags))
      )
        (map-set connections
          { user: user, connection-with: connection-with }
          (merge connection { 
            tags: updated-tags,
            updated-at: timestamp
          })
        )
        
        (ok true)
      )
    )
  )
)

;; Update notes for a connection
(define-public (update-notes 
  (connection-with principal) 
  (notes (optional (string-utf8 500)))
)
  (let (
    (user tx-sender)
  )
    ;; Verify connection exists
    (asserts! (connection-exists user connection-with) ERR-CONNECTION-NOT-FOUND)
    
    ;; Update notes
    (let (
      (connection (unwrap! (get-connection user connection-with) ERR-CONNECTION-NOT-FOUND))
      (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
    )
      (map-set connections
        { user: user, connection-with: connection-with }
        (merge connection { 
          notes: notes,
          updated-at: timestamp
        })
      )
      
      (ok true)
    )
  )
)

;; Archive a connection
(define-public (archive-connection (connection-with principal))
  (let (
    (user tx-sender)
  )
    ;; Verify connection exists and is in CONNECTED state
    (asserts! (are-connected user connection-with) ERR-CONNECTION-NOT-FOUND)
    
    ;; Update connection state to ARCHIVED
    (let (
      (connection (unwrap! (get-connection user connection-with) ERR-CONNECTION-NOT-FOUND))
      (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
    )
      (map-set connections
        { user: user, connection-with: connection-with }
        (merge connection { 
          state: STATE-ARCHIVED,
          updated-at: timestamp
        })
      )
      
      (ok true)
    )
  )
)

;; Restore an archived connection
(define-public (restore-connection (connection-with principal))
  (let (
    (user tx-sender)
  )
    ;; Verify connection exists
    (let (
      (connection (unwrap! (get-connection user connection-with) ERR-CONNECTION-NOT-FOUND))
    )
      ;; Verify connection is in ARCHIVED state
      (asserts! (is-eq (get state connection) STATE-ARCHIVED) ERR-INVALID-STATE)
      
      ;; Update connection state to CONNECTED
      (let (
        (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
      )
        (map-set connections
          { user: user, connection-with: connection-with }
          (merge connection { 
            state: STATE-CONNECTED,
            updated-at: timestamp
          })
        )
        
        (ok true)
      )
    )
  )
)

;; Remove a connection entirely
(define-public (remove-connection (connection-with principal))
  (let (
    (user tx-sender)
  )
    ;; Verify connection exists
    (asserts! (connection-exists user connection-with) ERR-CONNECTION-NOT-FOUND)
    
    ;; Delete connection records for both users
    (map-delete connections { user: user, connection-with: connection-with })
    
    ;; Remove from user's connections list
    (let (
      (user-conn-data (default-to { connections: (list) } (map-get? user-connections { user: user })))
    )
      (map-set user-connections
        { user: user }
        { connections: (remove-from-list (get connections user-conn-data) connection-with) }
      )
      
      (ok true)
    )
  )
)