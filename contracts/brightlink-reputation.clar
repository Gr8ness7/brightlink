;; brightlink-reputation
;; A reputation system for the Brightlink professional networking platform
;; This contract allows users to receive verifiable endorsements and testimonials
;; for skills and professional achievements, creating a decentralized reputation system.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-USER-NOT-FOUND (err u1002))
(define-constant ERR-ALREADY-ENDORSED (err u1003))
(define-constant ERR-TESTIMONIAL-NOT-FOUND (err u1004))
(define-constant ERR-CANNOT-ENDORSE-SELF (err u1005))
(define-constant ERR-SKILL-NOT-FOUND (err u1006))
(define-constant ERR-ACHIEVEMENT-NOT-FOUND (err u1007))
(define-constant ERR-INVALID-ISSUER (err u1008))
(define-constant ERR-ACHIEVEMENT-EXISTS (err u1009))

;; Data space definitions

;; Map to track user profiles
(define-map user-profiles
  { user: principal }
  { 
    bio: (string-utf8 500),
    registration-time: uint,
    testimonial-count: uint,
    endorsement-count: uint
  }
)

;; Map to track skills endorsed for each user
(define-map user-skills
  { user: principal, skill: (string-utf8 64) }
  { 
    endorsement-count: uint,
    created-at: uint
  }
)

;; Map to track who has endorsed a user for a specific skill
(define-map skill-endorsements
  { user: principal, skill: (string-utf8 64), endorser: principal }
  { 
    endorsed-at: uint
  }
)

;; Map to store testimonials
(define-map testimonials
  { testimonial-id: uint }
  {
    from: principal,
    to: principal,
    content: (string-utf8 1000),
    created-at: uint
  }
)

;; Map to track user's achievements/credentials
(define-map user-achievements
  { user: principal, achievement-id: uint }
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    issuer: principal,
    issue-date: uint,
    expiration-date: (optional uint),
    verified: bool
  }
)

;; Counter for testimonial IDs
(define-data-var testimonial-id-counter uint u0)

;; Counter for achievement IDs
(define-data-var achievement-id-counter uint u0)

;; Private functions

;; Check if a user has a profile
(define-private (has-profile (user principal))
  (default-to false (map-get? user-profiles { user: user }))
)

;; Check if a user has endorsed another user for a specific skill
(define-private (has-endorsed (endorser principal) (user principal) (skill (string-utf8 64)))
  (is-some (map-get? skill-endorsements { user: user, skill: skill, endorser: endorser }))
)

;; Increment testimonial ID counter
(define-private (get-and-increment-testimonial-id)
  (let ((current-id (var-get testimonial-id-counter)))
    (var-set testimonial-id-counter (+ current-id u1))
    current-id
  )
)

;; Increment achievement ID counter
(define-private (get-and-increment-achievement-id)
  (let ((current-id (var-get achievement-id-counter)))
    (var-set achievement-id-counter (+ current-id u1))
    current-id
  )
)

;; Read-only functions

;; Get a user's profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Get information about a specific skill endorsement
(define-read-only (get-skill-info (user principal) (skill (string-utf8 64)))
  (map-get? user-skills { user: user, skill: skill })
)

;; Check if a user has been endorsed by a specific endorser for a skill
(define-read-only (check-endorsement (user principal) (skill (string-utf8 64)) (endorser principal))
  (map-get? skill-endorsements { user: user, skill: skill, endorser: endorser })
)

;; Get a testimonial by ID
(define-read-only (get-testimonial (testimonial-id uint))
  (map-get? testimonials { testimonial-id: testimonial-id })
)

;; Get testimonials for a user (this would be paginated in a real implementation)
;; For simplicity, we're returning a function that would be used with other methods to paginate
(define-read-only (get-testimonial-exists (testimonial-id uint))
  (is-some (map-get? testimonials { testimonial-id: testimonial-id }))
)

;; Get achievement information
(define-read-only (get-achievement (user principal) (achievement-id uint))
  (map-get? user-achievements { user: user, achievement-id: achievement-id })
)

;; Public functions

;; Register or update user profile
(define-public (register-profile (bio (string-utf8 500)))
  (let ((user tx-sender))
    (map-set user-profiles
      { user: user }
      { 
        bio: bio,
        registration-time: block-height,
        testimonial-count: (default-to u0 (get testimonial-count (map-get? user-profiles { user: user }))),
        endorsement-count: (default-to u0 (get endorsement-count (map-get? user-profiles { user: user })))
      }
    )
    (ok true)
  )
)

;; Endorse a user for a skill
(define-public (endorse-skill (user principal) (skill (string-utf8 64)))
  (let ((endorser tx-sender))
    (asserts! (not (is-eq endorser user)) ERR-CANNOT-ENDORSE-SELF)
    (asserts! (has-profile user) ERR-USER-NOT-FOUND)
    (asserts! (not (has-endorsed endorser user skill)) ERR-ALREADY-ENDORSED)
    
    ;; Update or create the skill record
    (let ((skill-data (map-get? user-skills { user: user, skill: skill })))
      (if (is-some skill-data)
        (let ((current-data (unwrap-panic skill-data)))
          (map-set user-skills
            { user: user, skill: skill }
            { 
              endorsement-count: (+ (get endorsement-count current-data) u1),
              created-at: (get created-at current-data)
            }
          )
        )
        (map-set user-skills
          { user: user, skill: skill }
          {
            endorsement-count: u1,
            created-at: block-height
          }
        )
      )
    )
    
    ;; Record the endorsement
    (map-set skill-endorsements
      { user: user, skill: skill, endorser: endorser }
      { endorsed-at: block-height }
    )
    
    ;; Update the user's total endorsement count
    (let ((profile (unwrap! (map-get? user-profiles { user: user }) ERR-USER-NOT-FOUND)))
      (map-set user-profiles
        { user: user }
        (merge profile { endorsement-count: (+ (get endorsement-count profile) u1) })
      )
    )
    
    (ok true)
  )
)

;; Create a testimonial for a user
(define-public (create-testimonial (recipient principal) (content (string-utf8 1000)))
  (let (
    (sender tx-sender)
    (testimonial-id (get-and-increment-testimonial-id))
  )
    (asserts! (has-profile recipient) ERR-USER-NOT-FOUND)
    (asserts! (not (is-eq sender recipient)) ERR-NOT-AUTHORIZED)
    
    ;; Store the testimonial
    (map-set testimonials
      { testimonial-id: testimonial-id }
      {
        from: sender,
        to: recipient,
        content: content,
        created-at: block-height
      }
    )
    
    ;; Update the recipient's testimonial count
    (let ((profile (unwrap! (map-get? user-profiles { user: recipient }) ERR-USER-NOT-FOUND)))
      (map-set user-profiles
        { user: recipient }
        (merge profile { testimonial-count: (+ (get testimonial-count profile) u1) })
      )
    )
    
    (ok testimonial-id)
  )
)

;; Issue an achievement/credential to a user
;; Only allowed by the specified issuer
(define-public (issue-achievement 
  (recipient principal) 
  (title (string-utf8 100)) 
  (description (string-utf8 500))
  (expiration-date (optional uint))
)
  (let (
    (issuer tx-sender)
    (achievement-id (get-and-increment-achievement-id))
  )
    (asserts! (has-profile recipient) ERR-USER-NOT-FOUND)
    
    ;; Store the achievement
    (map-set user-achievements
      { user: recipient, achievement-id: achievement-id }
      {
        title: title,
        description: description,
        issuer: issuer,
        issue-date: block-height,
        expiration-date: expiration-date,
        verified: true
      }
    )
    
    (ok achievement-id)
  )
)

;; Claim an achievement (to be verified later by the issuer)
(define-public (claim-achievement 
  (title (string-utf8 100))
  (description (string-utf8 500))
  (issuer principal)
  (expiration-date (optional uint))
)
  (let (
    (user tx-sender)
    (achievement-id (get-and-increment-achievement-id))
  )
    ;; Store the unverified achievement
    (map-set user-achievements
      { user: user, achievement-id: achievement-id }
      {
        title: title,
        description: description,
        issuer: issuer,
        issue-date: block-height,
        expiration-date: expiration-date,
        verified: false
      }
    )
    
    (ok achievement-id)
  )
)

;; Verify a claimed achievement
(define-public (verify-achievement (user principal) (achievement-id uint))
  (let (
    (issuer tx-sender)
    (achievement (unwrap! (map-get? user-achievements { user: user, achievement-id: achievement-id }) ERR-ACHIEVEMENT-NOT-FOUND))
  )
    (asserts! (is-eq issuer (get issuer achievement)) ERR-INVALID-ISSUER)
    (asserts! (not (get verified achievement)) ERR-ACHIEVEMENT-EXISTS)
    
    ;; Update the achievement to verified status
    (map-set user-achievements
      { user: user, achievement-id: achievement-id }
      (merge achievement { verified: true })
    )
    
    (ok true)
  )
)