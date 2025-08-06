;; Research Impact Tracking & Citation System
;; Tracks post-funding research outcomes and calculates impact metrics

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-INVALID-PARAMETER (err u102))
(define-constant ERR-DUPLICATE-ENTRY (err u103))
(define-constant ERR-CITATION-NOT-FOUND (err u104))
(define-constant ERR-INSUFFICIENT-EVIDENCE (err u105))

;; Data Maps
(define-map research-outcomes
    { proposal-id: uint }
    {
        total-publications: uint,
        total-citations: uint,
        total-patents: uint,
        impact-score: uint,
        last-updated: uint,
        verified: bool
    }
)

(define-map publications
    { proposal-id: uint, publication-id: uint }
    {
        title: (string-ascii 100),
        journal: (string-ascii 50),
        publication-date: uint,
        doi: (string-ascii 50),
        citation-count: uint,
        impact-factor: uint,
        verified: bool,
        submitter: principal
    }
)

(define-map citations
    { proposal-id: uint, publication-id: uint, citation-id: uint }
    {
        citing-paper: (string-ascii 100),
        citation-context: (string-ascii 200),
        citation-date: uint,
        verified: bool,
        submitter: principal
    }
)

(define-map patents
    { proposal-id: uint, patent-id: uint }
    {
        title: (string-ascii 100),
        patent-number: (string-ascii 30),
        filing-date: uint,
        grant-date: uint,
        inventors: (string-ascii 200),
        verified: bool,
        submitter: principal
    }
)

(define-map collaboration-networks
    { proposal-id: uint, collaborator: principal }
    {
        institution: (string-ascii 50),
        collaboration-type: (string-ascii 30),
        start-date: uint,
        active: bool
    }
)

(define-map impact-verifiers
    { verifier: principal }
    {
        field-expertise: (string-ascii 50),
        verification-count: uint,
        reputation-score: uint,
        active: bool
    }
)

(define-map verification-queue
    { proposal-id: uint, item-type: (string-ascii 20), item-id: uint }
    {
        pending-verifications: uint,
        required-verifications: uint,
        submitted-timestamp: uint
    }
)

(define-map retroactive-rewards
    { proposal-id: uint }
    {
        impact-multiplier: uint,
        reward-pool: uint,
        distributed: bool,
        calculation-date: uint
    }
)

;; Data Variables
(define-data-var publication-count uint u0)
(define-data-var citation-count uint u0)
(define-data-var patent-count uint u0)
(define-data-var min-verifications uint u3)
(define-data-var impact-calculation-window uint u52560) ;; ~1 year in blocks

;; Publication Management Functions
(define-public (submit-publication 
    (proposal-id uint)
    (title (string-ascii 100))
    (journal (string-ascii 50))
    (publication-date uint)
    (doi (string-ascii 50))
    (impact-factor uint))
    
    (let ((pub-id (+ u1 (var-get publication-count))))
        
        ;; Validate inputs
        (asserts! (> (len title) u0) ERR-INVALID-PARAMETER)
        (asserts! (> (len journal) u0) ERR-INVALID-PARAMETER)
        (asserts! (> publication-date u0) ERR-INVALID-PARAMETER)
        
        ;; Store publication
        (map-set publications
            { proposal-id: proposal-id, publication-id: pub-id }
            {
                title: title,
                journal: journal,
                publication-date: publication-date,
                doi: doi,
                citation-count: u0,
                impact-factor: impact-factor,
                verified: false,
                submitter: tx-sender
            }
        )
        
        ;; Add to verification queue
        (map-set verification-queue
            { proposal-id: proposal-id, item-type: "publication", item-id: pub-id }
            {
                pending-verifications: u0,
                required-verifications: (var-get min-verifications),
                submitted-timestamp: stacks-block-height
            }
        )
        
        (var-set publication-count pub-id)
        (ok pub-id)
    )
)

(define-public (submit-citation
    (proposal-id uint)
    (publication-id uint)
    (citing-paper (string-ascii 100))
    (citation-context (string-ascii 200))
    (citation-date uint))
    
    (let ((citation-id (+ u1 (var-get citation-count))))
        
        ;; Validate publication exists
        (asserts! (is-some (map-get? publications { proposal-id: proposal-id, publication-id: publication-id }))
                 ERR-PROPOSAL-NOT-FOUND)
        
        ;; Store citation
        (map-set citations
            { proposal-id: proposal-id, publication-id: publication-id, citation-id: citation-id }
            {
                citing-paper: citing-paper,
                citation-context: citation-context,
                citation-date: citation-date,
                verified: false,
                submitter: tx-sender
            }
        )
        
        ;; Add to verification queue
        (map-set verification-queue
            { proposal-id: proposal-id, item-type: "citation", item-id: citation-id }
            {
                pending-verifications: u0,
                required-verifications: (var-get min-verifications),
                submitted-timestamp: stacks-block-height
            }
        )
        
        (var-set citation-count citation-id)
        (ok citation-id)
    )
)

(define-public (submit-patent
    (proposal-id uint)
    (title (string-ascii 100))
    (patent-number (string-ascii 30))
    (filing-date uint)
    (grant-date uint)
    (inventors (string-ascii 200)))
    
    (let ((patent-id (+ u1 (var-get patent-count))))
        
        ;; Validate inputs
        (asserts! (> (len title) u0) ERR-INVALID-PARAMETER)
        (asserts! (> (len patent-number) u0) ERR-INVALID-PARAMETER)
        (asserts! (> filing-date u0) ERR-INVALID-PARAMETER)
        
        ;; Store patent
        (map-set patents
            { proposal-id: proposal-id, patent-id: patent-id }
            {
                title: title,
                patent-number: patent-number,
                filing-date: filing-date,
                grant-date: grant-date,
                inventors: inventors,
                verified: false,
                submitter: tx-sender
            }
        )
        
        ;; Add to verification queue
        (map-set verification-queue
            { proposal-id: proposal-id, item-type: "patent", item-id: patent-id }
            {
                pending-verifications: u0,
                required-verifications: (var-get min-verifications),
                submitted-timestamp: stacks-block-height
            }
        )
        
        (var-set patent-count patent-id)
        (ok patent-id)
    )
)

;; Verification System
(define-public (verify-impact-item
    (proposal-id uint)
    (item-type (string-ascii 20))
    (item-id uint))
    
    (let ((verifier-data (unwrap! (map-get? impact-verifiers { verifier: tx-sender }) ERR-NOT-AUTHORIZED))
          (queue-item (unwrap! (map-get? verification-queue { proposal-id: proposal-id, item-type: item-type, item-id: item-id })
                              ERR-CITATION-NOT-FOUND)))
        
        ;; Ensure verifier is active
        (asserts! (get active verifier-data) ERR-NOT-AUTHORIZED)
        
        ;; Update verification count
        (map-set verification-queue
            { proposal-id: proposal-id, item-type: item-type, item-id: item-id }
            (merge queue-item { pending-verifications: (+ (get pending-verifications queue-item) u1) })
        )
        
        ;; Update verifier stats
        (map-set impact-verifiers
            { verifier: tx-sender }
            (merge verifier-data { verification-count: (+ (get verification-count verifier-data) u1) })
        )
        
        ;; Check if enough verifications collected
        (if (>= (+ (get pending-verifications queue-item) u1) (get required-verifications queue-item))
            (finalize-verification proposal-id item-type item-id)
            (ok true)
        )
    )
)

(define-private (finalize-verification
    (proposal-id uint)
    (item-type (string-ascii 20))
    (item-id uint))
    
    (begin
        ;; Mark item as verified based on type
        (if (is-eq item-type "publication")
            (begin (unwrap-panic (update-publication-verification proposal-id item-id)) (ok true))
            (if (is-eq item-type "citation")
                (begin (unwrap-panic (update-citation-verification proposal-id item-id)) (ok true))
                (if (is-eq item-type "patent")
                    (begin (unwrap-panic (update-patent-verification proposal-id item-id)) (ok true))
                    (ok false)
                )
            )
        )
    )
)

(define-private (update-publication-verification (proposal-id uint) (publication-id uint))
    (match (map-get? publications { proposal-id: proposal-id, publication-id: publication-id })
        publication (begin
            (map-set publications
                { proposal-id: proposal-id, publication-id: publication-id }
                (merge publication { verified: true })
            )
            (recalculate-impact-score proposal-id)
        )
        (err ERR-PROPOSAL-NOT-FOUND)
    )
)

(define-private (update-citation-verification (proposal-id uint) (citation-id uint))
    (let ((updated (update-publication-citation-count proposal-id citation-id)))
        (recalculate-impact-score proposal-id)
    )
)

(define-private (update-patent-verification (proposal-id uint) (patent-id uint))
    (match (map-get? patents { proposal-id: proposal-id, patent-id: patent-id })
        patent (begin
            (map-set patents
                { proposal-id: proposal-id, patent-id: patent-id }
                (merge patent { verified: true })
            )
            (recalculate-impact-score proposal-id)
        )
        (err ERR-PROPOSAL-NOT-FOUND)
    )
)

;; Impact Calculation
(define-public (recalculate-impact-score (proposal-id uint))
    (let ((current-outcomes (default-to 
                            { total-publications: u0, total-citations: u0, total-patents: u0, 
                              impact-score: u0, last-updated: u0, verified: false }
                            (map-get? research-outcomes { proposal-id: proposal-id })))
          (pub-count (count-verified-publications proposal-id))
          (cite-count (count-verified-citations proposal-id))
          (patent-num (count-verified-patents proposal-id))
          (new-impact-score (calculate-composite-score pub-count cite-count patent-num)))
        
        (map-set research-outcomes
            { proposal-id: proposal-id }
            {
                total-publications: pub-count,
                total-citations: cite-count,
                total-patents: patent-num,
                impact-score: new-impact-score,
                last-updated: stacks-block-height,
                verified: true
            }
        )
        
        ;; Check for retroactive rewards eligibility
        (if (> new-impact-score u500) ;; High impact threshold
            (calculate-retroactive-reward proposal-id new-impact-score)
            (ok true)
        )
    )
)

(define-private (calculate-composite-score (pub-count uint) (cite-count uint) (patent-num uint))
    (+ (* pub-count u50)        ;; Base points per publication
       (* cite-count u10)       ;; Points per citation
       (* patent-num u100))     ;; Higher points for patents
)

;; Retroactive Reward System
(define-public (calculate-retroactive-reward (proposal-id uint) (impact-score uint))
    (let ((multiplier (/ impact-score u100))
          (base-reward u1000))
        
        (map-set retroactive-rewards
            { proposal-id: proposal-id }
            {
                impact-multiplier: multiplier,
                reward-pool: (* base-reward multiplier),
                distributed: false,
                calculation-date: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Helper Functions
(define-private (count-verified-publications (proposal-id uint))
    ;; Simplified - in real implementation would iterate through all publications
    u0
)

(define-private (count-verified-citations (proposal-id uint))
    ;; Simplified - in real implementation would iterate through all citations
    u0
)

(define-private (count-verified-patents (proposal-id uint))
    ;; Simplified - in real implementation would iterate through all patents
    u0
)

(define-private (update-publication-citation-count (proposal-id uint) (citation-id uint))
    ;; Simplified - would update citation count for related publication
    true
)

;; Collaboration Network Functions
(define-public (add-collaboration
    (proposal-id uint)
    (collaborator principal)
    (institution (string-ascii 50))
    (collaboration-type (string-ascii 30)))
    
    (begin
        (map-set collaboration-networks
            { proposal-id: proposal-id, collaborator: collaborator }
            {
                institution: institution,
                collaboration-type: collaboration-type,
                start-date: stacks-block-height,
                active: true
            }
        )
        (ok true)
    )
)

;; Verifier Management
(define-public (register-verifier
    (verifier principal)
    (field-expertise (string-ascii 50)))
    
    (begin
        (map-set impact-verifiers
            { verifier: verifier }
            {
                field-expertise: field-expertise,
                verification-count: u0,
                reputation-score: u100,
                active: true
            }
        )
        (ok true)
    )
)

;; Read-Only Functions
(define-read-only (get-research-outcomes (proposal-id uint))
    (map-get? research-outcomes { proposal-id: proposal-id })
)

(define-read-only (get-publication (proposal-id uint) (publication-id uint))
    (map-get? publications { proposal-id: proposal-id, publication-id: publication-id })
)

(define-read-only (get-citation (proposal-id uint) (publication-id uint) (citation-id uint))
    (map-get? citations { proposal-id: proposal-id, publication-id: publication-id, citation-id: citation-id })
)

(define-read-only (get-patent (proposal-id uint) (patent-id uint))
    (map-get? patents { proposal-id: proposal-id, patent-id: patent-id })
)

(define-read-only (get-verifier-status (verifier principal))
    (map-get? impact-verifiers { verifier: verifier })
)

(define-read-only (get-retroactive-reward (proposal-id uint))
    (map-get? retroactive-rewards { proposal-id: proposal-id })
)

(define-read-only (get-verification-status (proposal-id uint) (item-type (string-ascii 20)) (item-id uint))
    (map-get? verification-queue { proposal-id: proposal-id, item-type: item-type, item-id: item-id })
)

;; Admin Functions
(define-public (set-min-verifications (new-min uint))
    (begin
        (var-set min-verifications new-min)
        (ok true)
    )
)

(define-public (set-impact-window (new-window uint))
    (begin
        (var-set impact-calculation-window new-window)
        (ok true)
    )
)


