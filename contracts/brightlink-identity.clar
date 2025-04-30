;; brightlink-identity
;; 
;; This contract manages user profiles and professional identity information for the BrightLink
;; decentralized professional networking application. It allows users to create and control their
;; professional identity on-chain, including profile details, skills, and professional experiences.
;; The contract implements privacy controls that let users determine what information is public
;; versus shared only with connections.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-USER-NOT-FOUND (err u101))
(define-constant ERR-INVALID-FIELD (err u102))
(define-constant ERR-INVALID-PRIVACY-SETTING (err u103))
(define-constant ERR-PROFILE-ALREADY-EXISTS (err u104))
(define-constant ERR-INVALID-SKILL (err u105))
(define-constant ERR-SKILL-ALREADY-EXISTS (err u106))
(define-constant ERR-EXPERIENCE-NOT-FOUND (err u107))
(define-constant ERR-INVALID-DATE-FORMAT (err u108))

;; Privacy settings
(define-constant PRIVACY-PUBLIC u1)
(define-constant PRIVACY-CONNECTIONS-ONLY u2)
(define-constant PRIVACY-PRIVATE u3)

;; Data maps
;; Main profile data map storing basic user information
(define-map profiles
  { owner: principal }
  {
    name: (string-utf8 100),
    headline: (string-utf8 200),
    bio: (string-utf8 500),
    location: (string-utf8 100),
    created-at: uint,
    updated-at: uint,
    exists: bool
  }
)

;; Privacy settings for different profile fields
(define-map profile-privacy
  { owner: principal, field: (string-utf8 20) }
  { setting: uint } ;; 1=public, 2=connections-only, 3=private
)

;; User skills
(define-map user-skills
  { owner: principal, skill: (string-utf8 50) }
  { 
    endorsement-count: uint,
    added-at: uint
  }
)

;; Professional experience entries
(define-map professional-experiences
  { owner: principal, id: uint }
  {
    title: (string-utf8 100),
    company: (string-utf8 100),
    start-date: (string-utf8 10), ;; Format: YYYY-MM-DD
    end-date: (optional (string-utf8 10)), ;; Format: YYYY-MM-DD, none if current
    description: (string-utf8 500),
    privacy: uint
  }
)

;; Track the next ID to use for experience entries
(define-map user-next-experience-id
  { owner: principal }
  { next-id: uint }
)

;; ========================================
;; Private functions
;; ========================================

;; Validates a date string is in YYYY-MM-DD format
(define-private (validate-date (date (string-utf8 10)))
  (let (
    (length (string-utf8-length date))
  )
    (if (not (is-eq length u10))
      false
      (let (
        (has-hyphens (and
          (is-eq (unwrap-panic (element-at date u4)) "-")
          (is-eq (unwrap-panic (element-at date u7)) "-")
        ))
      )
        has-hyphens
      )
    )
  )
)

;; Gets the next experience ID for a user
(define-private (get-next-experience-id (user principal))
  (default-to u1 (get next-id (map-get? user-next-experience-id { owner: user })))
)

;; Increments the next experience ID for a user
(define-private (increment-experience-id (user principal))
  (let (
    (current-id (get-next-experience-id user))
  )
    (map-set user-next-experience-id
      { owner: user }
      { next-id: (+ current-id u1) }
    )
    current-id
  )
)

;; Validates a privacy setting value
(define-private (is-valid-privacy-setting (setting uint))
  (or
    (is-eq setting PRIVACY-PUBLIC)
    (is-eq setting PRIVACY-CONNECTIONS-ONLY)
    (is-eq setting PRIVACY-PRIVATE)
  )
)

;; ========================================
;; Read-only functions
;; ========================================

;; Check if a profile exists
(define-read-only (profile-exists (user principal))
  (default-to false (get exists (map-get? profiles { owner: user })))
)

;; Get a user's profile if accessible based on privacy settings
(define-read-only (get-profile (user principal))
  (if (profile-exists user)
    (let (
      (profile-data (unwrap-panic (map-get? profiles { owner: user })))
    )
      (ok profile-data)
    )
    ERR-USER-NOT-FOUND
  )
)

;; Get a user's privacy setting for a specific field
(define-read-only (get-privacy-setting (user principal) (field (string-utf8 20)))
  (default-to 
    { setting: PRIVACY-PUBLIC } ;; Default to public if not set
    (map-get? profile-privacy { owner: user, field: field })
  )
)

;; Get a list of skills for a user
(define-read-only (get-skill (user principal) (skill (string-utf8 50)))
  (map-get? user-skills { owner: user, skill: skill })
)

;; Get a specific experience entry
(define-read-only (get-experience (user principal) (id uint))
  (let (
    (experience (map-get? professional-experiences { owner: user, id: id }))
  )
    (if (is-some experience)
      (ok (unwrap-panic experience))
      ERR-EXPERIENCE-NOT-FOUND
    )
  )
)

;; ========================================
;; Public functions
;; ========================================

;; Create or update user profile
(define-public (create-or-update-profile 
  (name (string-utf8 100))
  (headline (string-utf8 200))
  (bio (string-utf8 500))
  (location (string-utf8 100))
)
  (let (
    (user tx-sender)
    (exists (profile-exists user))
    (current-time (unwrap-panic (get-block-info? time u0)))
  )
    (map-set profiles
      { owner: user }
      {
        name: name,
        headline: headline,
        bio: bio,
        location: location,
        created-at: (if exists 
                      (get created-at (unwrap-panic (map-get? profiles { owner: user })))
                      current-time),
        updated-at: current-time,
        exists: true
      }
    )
    (ok true)
  )
)

;; Update privacy setting for a profile field
(define-public (set-privacy-setting (field (string-utf8 20)) (setting uint))
  (if (is-valid-privacy-setting setting)
    (begin
      (map-set profile-privacy
        { owner: tx-sender, field: field }
        { setting: setting }
      )
      (ok true)
    )
    ERR-INVALID-PRIVACY-SETTING
  )
)

;; Add a skill to the user's profile
(define-public (add-skill (skill (string-utf8 50)))
  (let (
    (user tx-sender)
    (existing-skill (map-get? user-skills { owner: user, skill: skill }))
    (current-time (unwrap-panic (get-block-info? time u0)))
  )
    (if (is-some existing-skill)
      ERR-SKILL-ALREADY-EXISTS
      (begin
        (map-set user-skills
          { owner: user, skill: skill }
          {
            endorsement-count: u0,
            added-at: current-time
          }
        )
        (ok true)
      )
    )
  )
)

;; Remove a skill from the user's profile
(define-public (remove-skill (skill (string-utf8 50)))
  (let (
    (user tx-sender)
    (existing-skill (map-get? user-skills { owner: user, skill: skill }))
  )
    (if (is-some existing-skill)
      (begin
        (map-delete user-skills { owner: user, skill: skill })
        (ok true)
      )
      ERR-INVALID-SKILL
    )
  )
)

;; Add a professional experience entry
(define-public (add-experience
  (title (string-utf8 100))
  (company (string-utf8 100))
  (start-date (string-utf8 10))
  (end-date (optional (string-utf8 10)))
  (description (string-utf8 500))
  (privacy uint)
)
  (let (
    (user tx-sender)
    (experience-id (increment-experience-id user))
  )
    ;; Validate start-date format
    (if (not (validate-date start-date))
      ERR-INVALID-DATE-FORMAT
      ;; Validate end-date format if provided
      (if (and (is-some end-date) (not (validate-date (unwrap-panic end-date))))
        ERR-INVALID-DATE-FORMAT
        ;; Validate privacy setting
        (if (not (is-valid-privacy-setting privacy))
          ERR-INVALID-PRIVACY-SETTING
          (begin
            (map-set professional-experiences
              { owner: user, id: experience-id }
              {
                title: title,
                company: company,
                start-date: start-date,
                end-date: end-date,
                description: description,
                privacy: privacy
              }
            )
            (ok experience-id)
          )
        )
      )
    )
  )
)

;; Update a professional experience entry
(define-public (update-experience
  (id uint)
  (title (string-utf8 100))
  (company (string-utf8 100))
  (start-date (string-utf8 10))
  (end-date (optional (string-utf8 10)))
  (description (string-utf8 500))
  (privacy uint)
)
  (let (
    (user tx-sender)
    (experience (map-get? professional-experiences { owner: user, id: id }))
  )
    (if (is-none experience)
      ERR-EXPERIENCE-NOT-FOUND
      ;; Validate start-date format
      (if (not (validate-date start-date))
        ERR-INVALID-DATE-FORMAT
        ;; Validate end-date format if provided
        (if (and (is-some end-date) (not (validate-date (unwrap-panic end-date))))
          ERR-INVALID-DATE-FORMAT
          ;; Validate privacy setting
          (if (not (is-valid-privacy-setting privacy))
            ERR-INVALID-PRIVACY-SETTING
            (begin
              (map-set professional-experiences
                { owner: user, id: id }
                {
                  title: title,
                  company: company,
                  start-date: start-date,
                  end-date: end-date,
                  description: description,
                  privacy: privacy
                }
              )
              (ok true)
            )
          )
        )
      )
    )
  )
)

;; Delete a professional experience entry
(define-public (delete-experience (id uint))
  (let (
    (user tx-sender)
    (experience (map-get? professional-experiences { owner: user, id: id }))
  )
    (if (is-none experience)
      ERR-EXPERIENCE-NOT-FOUND
      (begin
        (map-delete professional-experiences { owner: user, id: id })
        (ok true)
      )
    )
  )
)

;; Endorse a skill for another user
(define-public (endorse-skill (skill-owner principal) (skill (string-utf8 50)))
  (let (
    (existing-skill (map-get? user-skills { owner: skill-owner, skill: skill }))
  )
    (if (is-none existing-skill)
      ERR-INVALID-SKILL
      (begin
        (map-set user-skills
          { owner: skill-owner, skill: skill }
          {
            endorsement-count: (+ u1 (get endorsement-count (unwrap-panic existing-skill))),
            added-at: (get added-at (unwrap-panic existing-skill))
          }
        )
        (ok true)
      )
    )
  )
)