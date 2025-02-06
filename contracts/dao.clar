;; Academic Research Funding DAO

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-VOTED (err u102))

;; Data Maps
(define-map proposals 
    { proposal-id: uint }
    {
        owner: principal,
        title: (string-ascii 50),
        funding-amount: uint,
        votes: uint,
        status: (string-ascii 10)
    }
)

(define-map votes 
    { voter: principal, proposal-id: uint } 
    { voted: bool }
)

(define-data-var proposal-count uint u0)

;; Public Functions
(define-public (submit-proposal (title (string-ascii 50)) (funding-amount uint))
    (let
        ((new-id (+ (var-get proposal-count) u1)))
        (map-set proposals
            { proposal-id: new-id }
            {
                owner: tx-sender,
                title: title,
                funding-amount: funding-amount,
                votes: u0,
                status: "active"
            }
        )
        (var-set proposal-count new-id)
        (ok new-id)
    )
)

(define-public (vote (proposal-id uint))
    (let
        ((proposal (unwrap! (map-get? proposals {proposal-id: proposal-id}) (err ERR-PROPOSAL-NOT-FOUND)))
         (has-voted (default-to {voted: false} (map-get? votes {voter: tx-sender, proposal-id: proposal-id}))))
        (asserts! (not (get voted has-voted)) (err ERR-ALREADY-VOTED))
        (map-set votes {voter: tx-sender, proposal-id: proposal-id} {voted: true})
        (map-set proposals 
            {proposal-id: proposal-id}
            (merge proposal {votes: (+ (get votes proposal) u1)})
        )
        (ok true)
    )
)

;; Read Only Functions
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals {proposal-id: proposal-id})
)

(define-read-only (get-proposal-count)
    (var-get proposal-count)
)




;; Add to data maps
(define-map proposal-deadlines 
    { proposal-id: uint }
    { deadline: uint }
)

;; Add deadline parameter to submit-proposal
(define-public (set-proposal-deadline (proposal-id uint) (blocks uint))
    (let ((current-block stacks-block-height))
        (map-set proposal-deadlines
            { proposal-id: proposal-id }
            { deadline: (+ current-block blocks) }
        )
        (ok true)
    )
)




(define-map comments
    { proposal-id: uint, comment-id: uint }
    {
        author: principal,
        content: (string-ascii 200),
        timestamp: uint
    }
)

(define-map comment-counts
    { proposal-id: uint }
    { count: uint }
)

(define-read-only (get-comment-count (proposal-id uint))
    (some (get count (default-to { count: u0 } (map-get? comment-counts { proposal-id: proposal-id }))))
)

(define-public (add-comment (proposal-id uint) (content (string-ascii 200)))
    (let ((comment-id (+ u1 (default-to u0 (get-comment-count proposal-id)))))
        (map-set comments
            { proposal-id: proposal-id, comment-id: comment-id }
            { author: tx-sender, content: content, timestamp: stacks-block-height }
        )
        (ok true)
    )
)






(define-map proposal-updates
    { proposal-id: uint, update-id: uint }
    {
        content: (string-ascii 200),
        timestamp: uint
    }
)

(define-map update-counts
    { proposal-id: uint }
    { count: uint }
)

(define-read-only (get-update-count (proposal-id uint))
    (get count (map-get? update-counts { proposal-id: proposal-id }))
)

(define-public (add-proposal-update (proposal-id uint) (content (string-ascii 200)))
    (let ((update-id (+ u1 (default-to u0 (get-update-count proposal-id)))))
        (map-set proposal-updates
            { proposal-id: proposal-id, update-id: update-id }
            { content: content, timestamp: stacks-block-height }
        )
        (ok true)
    )
)



(define-map ratings
    { proposal-id: uint, rater: principal }
    { rating: uint }  ;; Rating from 1-5
)

(define-public (rate-proposal (proposal-id uint) (rating uint))
    (begin
        (asserts! (and (>= rating u1) (<= rating u5)) (err u403))
        (map-set ratings
            { proposal-id: proposal-id, rater: tx-sender }
            { rating: rating }
        )
        (ok true)
    )
)
