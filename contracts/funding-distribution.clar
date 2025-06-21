(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-FUNDS (err u200))
(define-constant ERR-MILESTONE-NOT-FOUND (err u201))
(define-constant ERR-MILESTONE-NOT-COMPLETED (err u202))
(define-constant ERR-ALREADY-DISTRIBUTED (err u203))
(define-constant ERR-INSUFFICIENT-APPROVALS (err u204))

(define-map funding-pool
    { proposal-id: uint }
    {
        total-funded: uint,
        distributed: uint,
        locked: bool
    }
)

(define-map milestone-distributions
    { proposal-id: uint, milestone-id: uint }
    {
        amount: uint,
        distributed: bool,
        approvals: uint,
        required-approvals: uint
    }
)

(define-map distribution-approvals
    { proposal-id: uint, milestone-id: uint, approver: principal }
    { approved: bool }
)

(define-map contributor-funds
    { proposal-id: uint, contributor: principal }
    { amount: uint }
)

(define-data-var total-pool uint u0)
(define-data-var min-approvers uint u3)

(define-public (contribute-to-proposal (proposal-id uint) (amount uint))
    (let 
        ((current-pool (default-to {total-funded: u0, distributed: u0, locked: false} 
                                  (map-get? funding-pool {proposal-id: proposal-id})))
         (contributor-amount (default-to {amount: u0} 
                                       (map-get? contributor-funds {proposal-id: proposal-id, contributor: tx-sender}))))
        
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        (map-set funding-pool
            {proposal-id: proposal-id}
            {
                total-funded: (+ (get total-funded current-pool) amount),
                distributed: (get distributed current-pool),
                locked: (get locked current-pool)
            }
        )
        
        (map-set contributor-funds
            {proposal-id: proposal-id, contributor: tx-sender}
            {amount: (+ (get amount contributor-amount) amount)}
        )
        
        (var-set total-pool (+ (var-get total-pool) amount))
        (ok true)
    )
)

(define-public (setup-milestone-distribution (proposal-id uint) (milestone-id uint) (amount uint))
    (let ((pool (unwrap! (map-get? funding-pool {proposal-id: proposal-id}) ERR-PROPOSAL-NOT-FOUND)))
        
        (asserts! (<= amount (- (get total-funded pool) (get distributed pool))) ERR-INSUFFICIENT-FUNDS)
        
        (map-set milestone-distributions
            {proposal-id: proposal-id, milestone-id: milestone-id}
            {
                amount: amount,
                distributed: false,
                approvals: u0,
                required-approvals: (var-get min-approvers)
            }
        )
        (ok true)
    )
)

(define-public (approve-milestone-distribution (proposal-id uint) (milestone-id uint))
    (let 
        ((distribution (unwrap! (map-get? milestone-distributions {proposal-id: proposal-id, milestone-id: milestone-id}) 
                               ERR-MILESTONE-NOT-FOUND))
         (has-approved (default-to {approved: false} 
                                  (map-get? distribution-approvals {proposal-id: proposal-id, milestone-id: milestone-id, approver: tx-sender}))))
        
        (asserts! (not (get approved has-approved)) ERR-NOT-AUTHORIZED)
        
        (map-set distribution-approvals
            {proposal-id: proposal-id, milestone-id: milestone-id, approver: tx-sender}
            {approved: true}
        )
        
        (map-set milestone-distributions
            {proposal-id: proposal-id, milestone-id: milestone-id}
            (merge distribution {approvals: (+ (get approvals distribution) u1)})
        )
        
        (ok true)
    )
)

(define-public (distribute-milestone-funds (proposal-id uint) (milestone-id uint) (recipient principal))
    (let 
        ((distribution (unwrap! (map-get? milestone-distributions {proposal-id: proposal-id, milestone-id: milestone-id}) 
                               ERR-MILESTONE-NOT-FOUND))
         (pool (unwrap! (map-get? funding-pool {proposal-id: proposal-id}) ERR-PROPOSAL-NOT-FOUND)))
        
        (asserts! (not (get distributed distribution)) ERR-ALREADY-DISTRIBUTED)
        (asserts! (>= (get approvals distribution) (get required-approvals distribution)) ERR-INSUFFICIENT-APPROVALS)
        (asserts! (not (get locked pool)) ERR-NOT-AUTHORIZED)
        
        (try! (as-contract (stx-transfer? (get amount distribution) tx-sender recipient)))
        
        (map-set milestone-distributions
            {proposal-id: proposal-id, milestone-id: milestone-id}
            (merge distribution {distributed: true})
        )
        
        (map-set funding-pool
            {proposal-id: proposal-id}
            (merge pool {distributed: (+ (get distributed pool) (get amount distribution))})
        )
        
        (ok true)
    )
)

(define-public (emergency-lock-funds (proposal-id uint))
    (let ((pool (unwrap! (map-get? funding-pool {proposal-id: proposal-id}) ERR-PROPOSAL-NOT-FOUND)))
        (map-set funding-pool
            {proposal-id: proposal-id}
            (merge pool {locked: true})
        )
        (ok true)
    )
)

(define-public (refund-contributor (proposal-id uint))
    (let 
        ((pool (unwrap! (map-get? funding-pool {proposal-id: proposal-id}) ERR-PROPOSAL-NOT-FOUND))
         (contribution (unwrap! (map-get? contributor-funds {proposal-id: proposal-id, contributor: tx-sender}) 
                               ERR-NOT-AUTHORIZED)))
        
        (asserts! (get locked pool) ERR-NOT-AUTHORIZED)
        (asserts! (> (get amount contribution) u0) ERR-INSUFFICIENT-FUNDS)
        
        (try! (as-contract (stx-transfer? (get amount contribution) tx-sender tx-sender)))
        
        (map-delete contributor-funds {proposal-id: proposal-id, contributor: tx-sender})
        
        (map-set funding-pool
            {proposal-id: proposal-id}
            (merge pool {total-funded: (- (get total-funded pool) (get amount contribution))})
        )
        
        (ok true)
    )
)

(define-public (set-min-approvers (new-min uint))
    (begin
        (var-set min-approvers new-min)
        (ok true)
    )
)

(define-read-only (get-funding-pool (proposal-id uint))
    (map-get? funding-pool {proposal-id: proposal-id})
)

(define-read-only (get-milestone-distribution (proposal-id uint) (milestone-id uint))
    (map-get? milestone-distributions {proposal-id: proposal-id, milestone-id: milestone-id})
)

(define-read-only (get-contributor-amount (proposal-id uint) (contributor principal))
    (map-get? contributor-funds {proposal-id: proposal-id, contributor: contributor})
)

(define-read-only (get-total-pool)
    (var-get total-pool)
)

(define-read-only (calculate-available-funds (proposal-id uint))
    (match (map-get? funding-pool {proposal-id: proposal-id})
        pool (ok (- (get total-funded pool) (get distributed pool)))
        ERR-PROPOSAL-NOT-FOUND
    )
)

(define-read-only (check-distribution-ready (proposal-id uint) (milestone-id uint))
    (match (map-get? milestone-distributions {proposal-id: proposal-id, milestone-id: milestone-id})
        distribution (ok (and 
                           (not (get distributed distribution))
                           (>= (get approvals distribution) (get required-approvals distribution))))
        ERR-MILESTONE-NOT-FOUND
    )
)