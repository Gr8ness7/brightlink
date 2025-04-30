;; brightlink-followups
;; 
;; This contract facilitates the management of professional follow-up tasks and reminders.
;; It allows users to create, track, update, and delete follow-up tasks related to their
;; professional connections, helping professionals maintain organized networking efforts
;; while keeping their interaction data private and under their control.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-FOLLOWUP-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATUS (err u102))
(define-constant ERR-INVALID-RECURRENCE (err u103))
(define-constant ERR-INVALID-DEADLINE (err u104))
(define-constant ERR-FOLLOWUP-LIMIT-REACHED (err u105))

;; Constants
(define-constant MAX-FOLLOWUPS-PER-USER u100)
(define-constant STATUS-PENDING u1)
(define-constant STATUS-COMPLETED u2)
(define-constant STATUS-CANCELLED u3)

;; Data mappings
;; Follow-up tasks are stored with a composite key of user-address and follow-up-id
(define-map followups
  { owner: principal, id: uint }
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    contact-name: (string-utf8 100),
    deadline: uint,
    status: uint,
    recurrence: (optional uint),  ;; Period in blocks, none for one-time followups
    created-at: uint,
    last-updated: uint
  }
)

;; Track the next available ID for each user
(define-map user-followup-count principal uint)

;; Track all followup IDs for a user
(define-map user-followup-ids 
  principal 
  (list 100 uint)  ;; List of up to 100 followup IDs
)

;; Private functions

;; Get the next available ID for a user and increment the counter
(define-private (get-and-increment-id (user principal))
  (let ((current-id (default-to u0 (map-get? user-followup-count user))))
    (map-set user-followup-count user (+ current-id u1))
    current-id
  )
)

;; Add a followup ID to the user's list of IDs
(define-private (add-followup-id (user principal) (id uint))
  (let ((current-ids (default-to (list) (map-get? user-followup-ids user))))
    (map-set user-followup-ids user (append current-ids id))
  )
)

;; Remove a followup ID from the user's list of IDs
(define-private (remove-followup-id (user principal) (id uint))
  (let ((current-ids (default-to (list) (map-get? user-followup-ids user))))
    (map-set user-followup-ids user (filter remove-id current-ids))
  )
  (ok true)
  
  ;; Helper function to filter out the specific ID
  (define-private (remove-id (item uint))
    (not (is-eq item id))
  )
)

;; Validate status input
(define-private (is-valid-status (status uint))
  (or
    (is-eq status STATUS-PENDING)
    (is-eq status STATUS-COMPLETED)
    (is-eq status STATUS-CANCELLED)
  )
)

;; Read-only functions

;; Get a specific follow-up task
(define-read-only (get-followup (owner principal) (id uint))
  (map-get? followups { owner: owner, id: id })
)

;; Get all follow-up IDs for a user
(define-read-only (get-followup-ids (user principal))
  (default-to (list) (map-get? user-followup-ids user))
)

;; Check if a deadline is in the future
(define-read-only (is-deadline-valid (deadline uint))
  (> deadline block-height)
)

;; Public functions

;; Create a new follow-up task
(define-public (create-followup 
    (title (string-utf8 100))
    (description (string-utf8 500))
    (contact-name (string-utf8 100))
    (deadline uint)
    (recurrence (optional uint)))
  (let 
    (
      (user tx-sender)
      (current-ids (default-to (list) (map-get? user-followup-ids user)))
      (followup-id (get-and-increment-id user))
    )
    
    ;; Check if user has reached their followup limit
    (asserts! (< (len current-ids) MAX-FOLLOWUPS-PER-USER) ERR-FOLLOWUP-LIMIT-REACHED)
    
    ;; Validate the deadline is in the future
    (asserts! (is-deadline-valid deadline) ERR-INVALID-DEADLINE)
    
    ;; Validate recurrence if present
    (asserts! 
      (match recurrence
        rec (> rec u0)
        true
      )
      ERR-INVALID-RECURRENCE
    )
    
    ;; Add the new followup
    (map-set followups
      { owner: user, id: followup-id }
      {
        title: title,
        description: description,
        contact-name: contact-name,
        deadline: deadline,
        status: STATUS-PENDING,
        recurrence: recurrence,
        created-at: block-height,
        last-updated: block-height
      }
    )
    
    ;; Add the ID to the user's list
    (add-followup-id user followup-id)
    
    (ok followup-id)
  )
)

;; Update an existing follow-up task
(define-public (update-followup
    (id uint)
    (title (string-utf8 100))
    (description (string-utf8 500))
    (contact-name (string-utf8 100))
    (deadline uint)
    (status uint)
    (recurrence (optional uint)))
  (let
    (
      (user tx-sender)
      (followup-data (map-get? followups { owner: user, id: id }))
    )
    
    ;; Check the followup exists and belongs to the sender
    (asserts! (is-some followup-data) ERR-FOLLOWUP-NOT-FOUND)
    
    ;; Validate the status
    (asserts! (is-valid-status status) ERR-INVALID-STATUS)
    
    ;; Validate the deadline
    (asserts! (is-deadline-valid deadline) ERR-INVALID-DEADLINE)
    
    ;; Validate recurrence if present
    (asserts! 
      (match recurrence
        rec (> rec u0)
        true
      )
      ERR-INVALID-RECURRENCE
    )
    
    ;; Update the followup
    (map-set followups
      { owner: user, id: id }
      {
        title: title,
        description: description,
        contact-name: contact-name,
        deadline: deadline,
        status: status,
        recurrence: recurrence,
        created-at: (get created-at (unwrap-panic followup-data)),
        last-updated: block-height
      }
    )
    
    (ok true)
  )
)

;; Delete a follow-up task
(define-public (delete-followup (id uint))
  (let
    (
      (user tx-sender)
      (followup-data (map-get? followups { owner: user, id: id }))
    )
    
    ;; Check the followup exists and belongs to the sender
    (asserts! (is-some followup-data) ERR-FOLLOWUP-NOT-FOUND)
    
    ;; Delete the followup
    (map-delete followups { owner: user, id: id })
    
    ;; Remove ID from user's list
    (remove-followup-id user id)
    
    (ok true)
  )
)

;; Mark a follow-up as complete
(define-public (complete-followup (id uint))
  (let
    (
      (user tx-sender)
      (followup-data (map-get? followups { owner: user, id: id }))
    )
    
    ;; Check the followup exists and belongs to the sender
    (asserts! (is-some followup-data) ERR-FOLLOWUP-NOT-FOUND)
    
    (let 
      (
        (unwrapped-data (unwrap-panic followup-data))
        (recurrence-value (get recurrence unwrapped-data))
      )
      
      ;; Handle recurring followups
      (match recurrence-value
        rec-period (
          ;; For recurring followups, update deadline instead of marking complete
          (map-set followups
            { owner: user, id: id }
            {
              title: (get title unwrapped-data),
              description: (get description unwrapped-data),
              contact-name: (get contact-name unwrapped-data),
              deadline: (+ block-height rec-period),
              status: STATUS-PENDING,
              recurrence: (some rec-period),
              created-at: (get created-at unwrapped-data),
              last-updated: block-height
            }
          )
        )
        ;; For one-time followups, mark as completed
        (map-set followups
          { owner: user, id: id }
          {
            title: (get title unwrapped-data),
            description: (get description unwrapped-data),
            contact-name: (get contact-name unwrapped-data),
            deadline: (get deadline unwrapped-data),
            status: STATUS-COMPLETED,
            recurrence: none,
            created-at: (get created-at unwrapped-data),
            last-updated: block-height
          }
        )
      )
      
      (ok true)
    )
  )
)

;; Cancel a follow-up task
(define-public (cancel-followup (id uint))
  (let
    (
      (user tx-sender)
      (followup-data (map-get? followups { owner: user, id: id }))
    )
    
    ;; Check the followup exists and belongs to the sender
    (asserts! (is-some followup-data) ERR-FOLLOWUP-NOT-FOUND)
    
    (let 
      (
        (unwrapped-data (unwrap-panic followup-data))
      )
      (map-set followups
        { owner: user, id: id }
        {
          title: (get title unwrapped-data),
          description: (get description unwrapped-data),
          contact-name: (get contact-name unwrapped-data),
          deadline: (get deadline unwrapped-data),
          status: STATUS-CANCELLED,
          recurrence: (get recurrence unwrapped-data),
          created-at: (get created-at unwrapped-data),
          last-updated: block-height
        }
      )
      
      (ok true)
    )
  )
)