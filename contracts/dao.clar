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
