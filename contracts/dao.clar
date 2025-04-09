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

;; Define tags map
(define-map proposal-tags
    { proposal-id: uint, tag: (string-ascii 20) }
    { exists: bool }
)

;; Add a tag to a proposal
(define-public (add-tag (proposal-id uint) (tag (string-ascii 20)))
    (let ((proposal (unwrap! (map-get? proposals {proposal-id: proposal-id}) (err ERR-PROPOSAL-NOT-FOUND))))
        ;; Only proposal owner can add tags
        (asserts! (is-eq tx-sender (get owner proposal)) (err ERR-NOT-AUTHORIZED))
        
        ;; Add the tag
        (map-set proposal-tags
            { proposal-id: proposal-id, tag: tag }
            { exists: true }
        )
        
        (ok true)
    )
)

;; Remove a tag from a proposal
(define-public (remove-tag (proposal-id uint) (tag (string-ascii 20)))
    (let ((proposal (unwrap! (map-get? proposals {proposal-id: proposal-id}) (err ERR-PROPOSAL-NOT-FOUND))))
        ;; Only proposal owner can remove tags
        (asserts! (is-eq tx-sender (get owner proposal)) (err ERR-NOT-AUTHORIZED))
        
        ;; Remove the tag
        (map-delete proposal-tags { proposal-id: proposal-id, tag: tag })
        
        (ok true)
    )
)

;; Check if a proposal has a specific tag
(define-read-only (has-tag (proposal-id uint) (tag (string-ascii 20)))
    (is-some (map-get? proposal-tags { proposal-id: proposal-id, tag: tag }))
)


;; Define collaborators map
(define-map proposal-collaborators
    { proposal-id: uint, collaborator: principal }
    { approved: bool }
)

;; Add a collaborator to a proposal
(define-public (add-collaborator (proposal-id uint) (collaborator principal))
    (let ((proposal (unwrap! (map-get? proposals {proposal-id: proposal-id}) (err ERR-PROPOSAL-NOT-FOUND))))
        ;; Only proposal owner can add collaborators
        (asserts! (is-eq tx-sender (get owner proposal)) (err ERR-NOT-AUTHORIZED))
        
        ;; Add the collaborator
        (map-set proposal-collaborators
            { proposal-id: proposal-id, collaborator: collaborator }
            { approved: false }
        )
        
        (ok true)
    )
)

;; Accept collaboration invitation
(define-public (accept-collaboration (proposal-id uint))
    (let ((collaboration (unwrap! (map-get? proposal-collaborators { proposal-id: proposal-id, collaborator: tx-sender }) (err u301))))
        ;; Update collaboration status
        (map-set proposal-collaborators
            { proposal-id: proposal-id, collaborator: tx-sender }
            { approved: true }
        )
        
        (ok true)
    )
)

;; Check if a principal is a collaborator on a proposal
(define-read-only (is-collaborator (proposal-id uint) (user principal))
    (match (map-get? proposal-collaborators { proposal-id: proposal-id, collaborator: user })
        collaboration (get approved collaboration)
        false
    )
)

;; Get all collaborators for a proposal
(define-read-only (get-collaborators (proposal-id uint))
    ;; This is a simplified version - in a real implementation, you'd need to iterate through all principals
    ;; which isn't directly possible in Clarity. You'd need an indexing service or off-chain component.
    (ok "Collaborators would be listed here")
)


;; Define proposal status constants
(define-constant STATUS-DRAFT "draft")
(define-constant STATUS-ACTIVE "active")
(define-constant STATUS-FUNDED "funded")
(define-constant STATUS-COMPLETED "completed")
(define-constant STATUS-REJECTED "rejected")

;; Update proposal status
(define-public (update-proposal-status (proposal-id uint) (new-status (string-ascii 10)))
    (let ((proposal (unwrap! (map-get? proposals {proposal-id: proposal-id}) (err ERR-PROPOSAL-NOT-FOUND))))
        ;; Only proposal owner can update status
        (asserts! (is-eq tx-sender (get owner proposal)) (err ERR-NOT-AUTHORIZED))
                
        ;; Update the proposal status
        (map-set proposals 
            {proposal-id: proposal-id}
            (merge proposal {status: new-status})
        )
        
        (ok true)
    )
)

;; Helper function to validate status transitions
(define-read-only (is-valid-status-transition (current-status (string-ascii 10)) (new-status (string-ascii 10)))
    (or
        (and (is-eq current-status STATUS-DRAFT) (is-eq new-status STATUS-ACTIVE))
        (and (is-eq current-status STATUS-ACTIVE) (is-eq new-status STATUS-FUNDED))
        (and (is-eq current-status STATUS-ACTIVE) (is-eq new-status STATUS-REJECTED))
        (and (is-eq current-status STATUS-FUNDED) (is-eq new-status STATUS-COMPLETED))
        false
    )
)

;; Get proposals by status
(define-read-only (get-proposals-by-status (status (string-ascii 10)))
    ;; This is a simplified version - in a real implementation, you'd need to iterate through all proposals
    ;; which isn't directly possible in Clarity. You'd need an indexing service or off-chain component.
    (ok "Proposals with specified status would be listed here")
)


;; Define budget item map
(define-map budget-items
    { proposal-id: uint, item-id: uint }
    {
        description: (string-ascii 50),
        amount: uint,
        category: (string-ascii 20)
    }
)

(define-map budget-item-counts
    { proposal-id: uint }
    { count: uint }
)

;; Add a budget item to a proposal
(define-public (add-budget-item (proposal-id uint) (description (string-ascii 50)) (amount uint) (category (string-ascii 20)))
    (let 
        ((proposal (unwrap! (map-get? proposals {proposal-id: proposal-id}) (err ERR-PROPOSAL-NOT-FOUND)))
         (item-count (default-to { count: u0 } (map-get? budget-item-counts { proposal-id: proposal-id })))
         (new-item-id (+ u1 (get count item-count))))
        
        ;; Only proposal owner can add budget items
        (asserts! (is-eq tx-sender (get owner proposal)) (err ERR-NOT-AUTHORIZED))
        
        ;; Add the budget item
        (map-set budget-items
            { proposal-id: proposal-id, item-id: new-item-id }
            { description: description, amount: amount, category: category }
        )
        
        ;; Update item count
        (map-set budget-item-counts
            { proposal-id: proposal-id }
            { count: new-item-id }
        )
        
        (ok new-item-id)
    )
)



(define-map milestones 
    { proposal-id: uint, milestone-id: uint }
    {
        title: (string-ascii 50),
        description: (string-ascii 200),
        deadline: uint,
        funding-amount: uint,
        completed: bool
    }
)

(define-map milestone-counts
    { proposal-id: uint }
    { count: uint }
)

(define-public (add-milestone (proposal-id uint) (title (string-ascii 50)) (description (string-ascii 200)) (deadline uint) (funding-amount uint))
    (let 
        ((proposal (unwrap! (map-get? proposals {proposal-id: proposal-id}) (err ERR-PROPOSAL-NOT-FOUND)))
         (milestone-count (default-to { count: u0 } (map-get? milestone-counts { proposal-id: proposal-id })))
         (new-milestone-id (+ u1 (get count milestone-count))))
        
        (asserts! (is-eq tx-sender (get owner proposal)) (err ERR-NOT-AUTHORIZED))
        
        (map-set milestones
            { proposal-id: proposal-id, milestone-id: new-milestone-id }
            { 
                title: title,
                description: description,
                deadline: deadline,
                funding-amount: funding-amount,
                completed: false
            }
        )
        
        (map-set milestone-counts
            { proposal-id: proposal-id }
            { count: new-milestone-id }
        )
        
        (ok new-milestone-id)
    )
)

(define-public (complete-milestone (proposal-id uint) (milestone-id uint))
    (let 
        ((proposal (unwrap! (map-get? proposals {proposal-id: proposal-id}) (err ERR-NOT-AUTHORIZED)))
         (milestone (unwrap! (map-get? milestones {proposal-id: proposal-id, milestone-id: milestone-id}) (err ERR-NOT-AUTHORIZED))))
        
        (asserts! (is-eq tx-sender (get owner proposal)) (err ERR-NOT-AUTHORIZED))
        
        (map-set milestones
            { proposal-id: proposal-id, milestone-id: milestone-id }
            (merge milestone { completed: true })
        )
        
        (ok true)
    )
)



(define-map peer-reviews
    { proposal-id: uint, reviewer: principal }
    {
        technical-score: uint,
        impact-score: uint,
        feasibility-score: uint,
        feedback: (string-ascii 500),
        timestamp: uint,
        verified: bool
    }
)

(define-map verified-reviewers
    { reviewer: principal }
    { field: (string-ascii 50) }
)

(define-public (submit-peer-review 
    (proposal-id uint) 
    (technical-score uint)
    (impact-score uint)
    (feasibility-score uint)
    (feedback (string-ascii 500)))
    
    (let ((proposal (unwrap! (map-get? proposals {proposal-id: proposal-id}) (err ERR-PROPOSAL-NOT-FOUND))))
        ;; (asserts! (is-some (map-get? verified-reviewers {reviewer: tx-sender})) (err u501))
        ;; (asserts! (and (>= technical-score u1) (<= technical-score u10)) (err u502))
        ;; (asserts! (and (>= impact-score u1) (<= impact-score u10)) (err u502))
        ;; (asserts! (and (>= feasibility-score u1) (<= feasibility-score u10)) (err u502))
        
        (map-set peer-reviews
            { proposal-id: proposal-id, reviewer: tx-sender }
            {
                technical-score: technical-score,
                impact-score: impact-score,
                feasibility-score: feasibility-score,
                feedback: feedback,
                timestamp: stacks-block-height,
                verified: true
            }
        )
        
        (ok true)
    )
)