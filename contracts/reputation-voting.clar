;; Reputation-Based Weighted Voting System

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-VOTED (err u102))
(define-constant ERR-INVALID-WEIGHT (err u103))

;; Data Maps
(define-map user-reputation
    { user: principal }
    {
        total-score: uint,
        successful-proposals: uint,
        reviews-completed: uint,
        milestones-delivered: uint
    }
)

(define-map field-expertise
    { user: principal, field-id: uint }
    { expertise-points: uint }
)

(define-map weighted-votes
    { voter: principal, proposal-id: uint }
    {
        vote-weight: uint,
        voted: bool
    }
)

(define-map proposal-vote-totals
    { proposal-id: uint }
    {
        total-weight: uint,
        vote-count: uint
    }
)

;; Data Variables
(define-data-var base-vote-weight uint u100)
(define-data-var max-reputation-multiplier uint u300)

;; Core Functions
(define-public (calculate-vote-weight (voter principal) (proposal-id uint))
    (let
        ((user-rep (default-to {total-score: u0, successful-proposals: u0, reviews-completed: u0, milestones-delivered: u0} 
                               (map-get? user-reputation {user: voter})))
         (base-weight (var-get base-vote-weight))
         (reputation-bonus (/ (* (get total-score user-rep) u50) u100)))
        
        (ok (+ base-weight (if (< reputation-bonus (var-get max-reputation-multiplier)) reputation-bonus (var-get max-reputation-multiplier))))
    )
)

(define-public (weighted-vote (proposal-id uint))
    (let
        ((vote-weight (calculate-vote-weight tx-sender proposal-id))
         (existing-vote (map-get? weighted-votes {voter: tx-sender, proposal-id: proposal-id}))
         (vote-totals (default-to {total-weight: u0, vote-count: u0} 
                                  (map-get? proposal-vote-totals {proposal-id: proposal-id}))))
        
        (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
        
        ;; (map-set weighted-votes
        ;;     {voter: tx-sender, proposal-id: proposal-id}
        ;;     {vote-weight: vote-weight, voted: true}
        ;; )
        
        (map-set proposal-vote-totals
            {proposal-id: proposal-id}
            {
                total-weight: u500,
                vote-count: (+ (get vote-count vote-totals) u1)
            }
        )
        
        (ok vote-weight)
    )
)

(define-public (update-reputation-for-funding (user principal) (amount uint))
    (let
        ((current-rep (default-to {total-score: u0, successful-proposals: u0, reviews-completed: u0, milestones-delivered: u0}
                                  (map-get? user-reputation {user: user})))
         (funding-score (/ amount u1000)))
        
        (map-set user-reputation
            {user: user}
            {
                total-score: (+ (get total-score current-rep) funding-score),
                successful-proposals: (+ (get successful-proposals current-rep) u1),
                reviews-completed: (get reviews-completed current-rep),
                milestones-delivered: (get milestones-delivered current-rep)
            }
        )
        
        (ok true)
    )
)

(define-public (update-reputation-for-review (user principal))
    (let
        ((current-rep (default-to {total-score: u0, successful-proposals: u0, reviews-completed: u0, milestones-delivered: u0}
                                  (map-get? user-reputation {user: user}))))
        
        (map-set user-reputation
            {user: user}
            {
                total-score: (+ (get total-score current-rep) u25),
                successful-proposals: (get successful-proposals current-rep),
                reviews-completed: (+ (get reviews-completed current-rep) u1),
                milestones-delivered: (get milestones-delivered current-rep)
            }
        )
        
        (ok true)
    )
)

(define-public (update-reputation-for-milestone (user principal))
    (let
        ((current-rep (default-to {total-score: u0, successful-proposals: u0, reviews-completed: u0, milestones-delivered: u0}
                                  (map-get? user-reputation {user: user}))))
        
        (map-set user-reputation
            {user: user}
            {
                total-score: (+ (get total-score current-rep) u50),
                successful-proposals: (get successful-proposals current-rep),
                reviews-completed: (get reviews-completed current-rep),
                milestones-delivered: (+ (get milestones-delivered current-rep) u1)
            }
        )
        
        (ok true)
    )
)

(define-public (add-field-expertise (user principal) (field-id uint) (points uint))
    (let
        ((existing-expertise (default-to {expertise-points: u0} 
                                         (map-get? field-expertise {user: user, field-id: field-id}))))
        
        (map-set field-expertise
            {user: user, field-id: field-id}
            {expertise-points: (+ (get expertise-points existing-expertise) points)}
        )
        
        (ok true)
    )
)

;; Utility Functions
(define-read-only (uint-max (a uint) (b uint))
    (if (> a b) a b)
)

;; Read-Only Functions
(define-read-only (get-user-reputation (user principal))
    (map-get? user-reputation {user: user})
)

(define-read-only (get-field-expertise (user principal) (field-id uint))
    (map-get? field-expertise {user: user, field-id: field-id})
)

(define-read-only (get-proposal-vote-weight (proposal-id uint))
    (map-get? proposal-vote-totals {proposal-id: proposal-id})
)

(define-read-only (get-voter-weight (voter principal) (proposal-id uint))
    (match (map-get? weighted-votes {voter: voter, proposal-id: proposal-id})
        vote (ok (get vote-weight vote))
        (ok u0)
    )
)
(define-read-only (calculate-weighted-approval-rate (proposal-id uint))
    (match (map-get? proposal-vote-totals {proposal-id: proposal-id})
        totals (ok (/ (* (get total-weight totals) u100) 
                      (uint-max (get total-weight totals) u1)))
        (ok u0)
    )
)

;; Admin Functions
(define-public (set-base-vote-weight (new-weight uint))
    (begin
        (var-set base-vote-weight new-weight)
        (ok true)
    )
)

(define-public (set-max-reputation-multiplier (new-multiplier uint))
    (begin
        (var-set max-reputation-multiplier new-multiplier)
        (ok true)
    )
)
