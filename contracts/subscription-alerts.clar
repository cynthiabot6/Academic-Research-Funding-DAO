;; Research Proposal Subscription & Alert System
;; Enables personalized discovery and real-time notifications for research proposals

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-SUBSCRIPTION (err u200))
(define-constant ERR-SUBSCRIPTION-EXISTS (err u201))
(define-constant ERR-SUBSCRIPTION-NOT-FOUND (err u202))
(define-constant ERR-ALERT-NOT-FOUND (err u203))
(define-constant ERR-INVALID-FILTER (err u204))
(define-constant ERR-MAX-SUBSCRIPTIONS-REACHED (err u205))

;; Subscription type constants
(define-constant FIELD-SUBSCRIPTION "field")
(define-constant FUNDING-TIER-SUBSCRIPTION "funding-tier")
(define-constant KEYWORD-SUBSCRIPTION "keyword")
(define-constant AUTHOR-SUBSCRIPTION "author")
(define-constant INSTITUTION-SUBSCRIPTION "institution")

;; Alert priority constants
(define-constant PRIORITY-LOW u1)
(define-constant PRIORITY-MEDIUM u2)
(define-constant PRIORITY-HIGH u3)
(define-constant PRIORITY-CRITICAL u4)

;; Data Maps
(define-map user-subscriptions
    { user: principal, subscription-id: uint }
    {
        subscription-type: (string-ascii 20),
        filter-value: (string-ascii 50),
        min-funding-amount: uint,
        max-funding-amount: uint,
        priority-level: uint,
        active: bool,
        created-at: uint,
        match-count: uint
    }
)

(define-map subscription-counts
    { user: principal }
    { total-subscriptions: uint }
)

(define-map field-subscriptions
    { field-id: uint, subscriber: principal }
    { 
        min-funding: uint,
        max-funding: uint,
        notification-frequency: uint,
        active: bool
    }
)

(define-map keyword-subscriptions
    { keyword: (string-ascii 30), subscriber: principal }
    {
        case-sensitive: bool,
        exact-match: bool,
        priority: uint,
        created-at: uint
    }
)

(define-map author-followings
    { follower: principal, following: principal }
    {
        follow-proposals: bool,
        follow-updates: bool,
        follow-milestones: bool,
        notification-level: uint
    }
)

(define-map proposal-alerts
    { user: principal, proposal-id: uint, alert-id: uint }
    {
        subscription-id: uint,
        alert-type: (string-ascii 20),
        alert-message: (string-ascii 200),
        priority: uint,
        created-at: uint,
        read: bool,
        dismissed: bool
    }
)

(define-map alert-counts
    { user: principal }
    {
        total-alerts: uint,
        unread-alerts: uint,
        high-priority-alerts: uint
    }
)

(define-map trending-topics
    { topic: (string-ascii 30), time-period: uint }
    {
        proposal-count: uint,
        total-funding: uint,
        subscriber-count: uint,
        trending-score: uint
    }
)

(define-map notification-preferences
    { user: principal }
    {
        email-enabled: bool,
        push-enabled: bool,
        digest-frequency: uint,
        max-daily-notifications: uint,
        quiet-hours-start: uint,
        quiet-hours-end: uint
    }
)

(define-map saved-searches
    { user: principal, search-id: uint }
    {
        search-name: (string-ascii 30),
        search-query: (string-ascii 100),
        auto-subscribe: bool,
        last-executed: uint,
        result-count: uint
    }
)

;; Data Variables
(define-data-var subscription-id-counter uint u0)
(define-data-var alert-id-counter uint u0)
(define-data-var search-id-counter uint u0)
(define-data-var max-subscriptions-per-user uint u20)
(define-data-var global-notification-enabled bool true)

;; Subscription Management Functions
(define-public (create-field-subscription (field-id uint) (min-funding uint) (max-funding uint) (frequency uint))
    (let ((user tx-sender)
          (current-subscriptions (default-to {total-subscriptions: u0} 
                                           (map-get? subscription-counts {user: user}))))
        
        ;; Check subscription limits
        (asserts! (< (get total-subscriptions current-subscriptions) (var-get max-subscriptions-per-user)) 
                 ERR-MAX-SUBSCRIPTIONS-REACHED)
        
        ;; Check if subscription already exists
        (asserts! (is-none (map-get? field-subscriptions {field-id: field-id, subscriber: user}))
                 ERR-SUBSCRIPTION-EXISTS)
        
        ;; Validate funding range
        (asserts! (<= min-funding max-funding) ERR-INVALID-FILTER)
        
        ;; Create field subscription
        (map-set field-subscriptions
            {field-id: field-id, subscriber: user}
            {
                min-funding: min-funding,
                max-funding: max-funding,
                notification-frequency: frequency,
                active: true
            }
        )
        
        ;; Update subscription count
        (map-set subscription-counts
            {user: user}
            {total-subscriptions: (+ (get total-subscriptions current-subscriptions) u1)}
        )
        
        (ok true)
    )
)

(define-public (subscribe-to-keyword 
    (keyword (string-ascii 30)) 
    (case-sensitive bool) 
    (exact-match bool) 
    (priority uint))
    
    (let ((user tx-sender))
        
        ;; Validate priority level
        (asserts! (and (>= priority u1) (<= priority u4)) ERR-INVALID-FILTER)
        
        ;; Check if keyword subscription exists
        (asserts! (is-none (map-get? keyword-subscriptions {keyword: keyword, subscriber: user}))
                 ERR-SUBSCRIPTION-EXISTS)
        
        ;; Create keyword subscription
        (map-set keyword-subscriptions
            {keyword: keyword, subscriber: user}
            {
                case-sensitive: case-sensitive,
                exact-match: exact-match,
                priority: priority,
                created-at: stacks-block-height
            }
        )
        
        (ok true)
    )
)

(define-public (follow-researcher 
    (researcher principal) 
    (follow-proposals bool) 
    (follow-updates bool) 
    (follow-milestones bool)
    (notification-level uint))
    
    (let ((follower tx-sender))
        
        ;; Validate inputs
        (asserts! (not (is-eq follower researcher)) ERR-INVALID-SUBSCRIPTION)
        (asserts! (and (>= notification-level u1) (<= notification-level u3)) ERR-INVALID-FILTER)
        
        ;; Create author following
        (map-set author-followings
            {follower: follower, following: researcher}
            {
                follow-proposals: follow-proposals,
                follow-updates: follow-updates,
                follow-milestones: follow-milestones,
                notification-level: notification-level
            }
        )
        
        (ok true)
    )
)

(define-public (create-advanced-subscription
    (subscription-type (string-ascii 20))
    (filter-value (string-ascii 50))
    (min-funding uint)
    (max-funding uint)
    (priority uint))
    
    (let ((user tx-sender)
          (subscription-id (+ u1 (var-get subscription-id-counter)))
          (current-count (default-to {total-subscriptions: u0} 
                                   (map-get? subscription-counts {user: user}))))
        
        ;; Validate subscription type
        (asserts! (or (is-eq subscription-type FIELD-SUBSCRIPTION)
                      (is-eq subscription-type FUNDING-TIER-SUBSCRIPTION)
                      (is-eq subscription-type KEYWORD-SUBSCRIPTION)
                      (is-eq subscription-type AUTHOR-SUBSCRIPTION)
                      (is-eq subscription-type INSTITUTION-SUBSCRIPTION))
                 ERR-INVALID-SUBSCRIPTION)
        
        ;; Check limits
        (asserts! (< (get total-subscriptions current-count) (var-get max-subscriptions-per-user))
                 ERR-MAX-SUBSCRIPTIONS-REACHED)
        
        ;; Create subscription
        (map-set user-subscriptions
            {user: user, subscription-id: subscription-id}
            {
                subscription-type: subscription-type,
                filter-value: filter-value,
                min-funding-amount: min-funding,
                max-funding-amount: max-funding,
                priority-level: priority,
                active: true,
                created-at: stacks-block-height,
                match-count: u0
            }
        )
        
        ;; Update counters
        (var-set subscription-id-counter subscription-id)
        (map-set subscription-counts
            {user: user}
            {total-subscriptions: (+ (get total-subscriptions current-count) u1)}
        )
        
        (ok subscription-id)
    )
)

;; Alert Generation Functions
(define-public (generate-proposal-alert
    (target-user principal)
    (proposal-id uint)
    (subscription-id uint)
    (alert-type (string-ascii 20))
    (alert-message (string-ascii 200))
    (priority uint))
    
    (let ((alert-id (+ u1 (var-get alert-id-counter)))
          (current-alerts (default-to {total-alerts: u0, unread-alerts: u0, high-priority-alerts: u0}
                                     (map-get? alert-counts {user: target-user}))))
        
        ;; Check if global notifications are enabled
        (asserts! (var-get global-notification-enabled) ERR-NOT-AUTHORIZED)
        
        ;; Create alert
        (map-set proposal-alerts
            {user: target-user, proposal-id: proposal-id, alert-id: alert-id}
            {
                subscription-id: subscription-id,
                alert-type: alert-type,
                alert-message: alert-message,
                priority: priority,
                created-at: stacks-block-height,
                read: false,
                dismissed: false
            }
        )
        
        ;; Update alert counts
        (map-set alert-counts
            {user: target-user}
            {
                total-alerts: (+ (get total-alerts current-alerts) u1),
                unread-alerts: (+ (get unread-alerts current-alerts) u1),
                high-priority-alerts: (if (>= priority PRIORITY-HIGH)
                                        (+ (get high-priority-alerts current-alerts) u1)
                                        (get high-priority-alerts current-alerts))
            }
        )
        
        ;; Increment alert counter
        (var-set alert-id-counter alert-id)
        
        (ok alert-id)
    )
)

(define-public (mark-alert-as-read (proposal-id uint) (alert-id uint))
    (let ((user tx-sender)
          (alert (unwrap! (map-get? proposal-alerts {user: user, proposal-id: proposal-id, alert-id: alert-id})
                         ERR-ALERT-NOT-FOUND))
          (alert-counts-data (default-to {total-alerts: u0, unread-alerts: u0, high-priority-alerts: u0}
                                       (map-get? alert-counts {user: user}))))
        
        ;; Update alert as read
        (map-set proposal-alerts
            {user: user, proposal-id: proposal-id, alert-id: alert-id}
            (merge alert {read: true})
        )
        
        ;; Decrease unread count if not already read
        (if (not (get read alert))
            (map-set alert-counts
                {user: user}
                (merge alert-counts-data {unread-alerts: (- (get unread-alerts alert-counts-data) u1)})
            )
            false
        )
        
        (ok true)
    )
)

(define-public (dismiss-alert (proposal-id uint) (alert-id uint))
    (let ((user tx-sender)
          (alert (unwrap! (map-get? proposal-alerts {user: user, proposal-id: proposal-id, alert-id: alert-id})
                         ERR-ALERT-NOT-FOUND)))
        
        ;; Mark alert as dismissed
        (map-set proposal-alerts
            {user: user, proposal-id: proposal-id, alert-id: alert-id}
            (merge alert {dismissed: true})
        )
        
        (ok true)
    )
)

;; Saved Search Functions
(define-public (create-saved-search 
    (search-name (string-ascii 30))
    (search-query (string-ascii 100))
    (auto-subscribe bool))
    
    (let ((user tx-sender)
          (search-id (+ u1 (var-get search-id-counter))))
        
        ;; Validate inputs
        (asserts! (> (len search-name) u0) ERR-INVALID-FILTER)
        (asserts! (> (len search-query) u0) ERR-INVALID-FILTER)
        
        ;; Create saved search
        (map-set saved-searches
            {user: user, search-id: search-id}
            {
                search-name: search-name,
                search-query: search-query,
                auto-subscribe: auto-subscribe,
                last-executed: stacks-block-height,
                result-count: u0
            }
        )
        
        (var-set search-id-counter search-id)
        (ok search-id)
    )
)

;; Notification Preference Management
(define-public (update-notification-preferences
    (email-enabled bool)
    (push-enabled bool)
    (digest-frequency uint)
    (max-daily uint)
    (quiet-start uint)
    (quiet-end uint))
    
    (let ((user tx-sender))
        
        ;; Validate time ranges
        (asserts! (and (<= quiet-start u23) (<= quiet-end u23)) ERR-INVALID-FILTER)
        (asserts! (<= digest-frequency u7) ERR-INVALID-FILTER) ;; Max weekly digest
        
        (map-set notification-preferences
            {user: user}
            {
                email-enabled: email-enabled,
                push-enabled: push-enabled,
                digest-frequency: digest-frequency,
                max-daily-notifications: max-daily,
                quiet-hours-start: quiet-start,
                quiet-hours-end: quiet-end
            }
        )
        
        (ok true)
    )
)

;; Trending Topic Analysis
(define-public (update-trending-topic (topic (string-ascii 30)) (time-period uint) (funding-amount uint))
    (let ((existing-trend (default-to {proposal-count: u0, total-funding: u0, subscriber-count: u0, trending-score: u0}
                                    (map-get? trending-topics {topic: topic, time-period: time-period}))))
        
        (map-set trending-topics
            {topic: topic, time-period: time-period}
            {
                proposal-count: (+ (get proposal-count existing-trend) u1),
                total-funding: (+ (get total-funding existing-trend) funding-amount),
                subscriber-count: (get subscriber-count existing-trend),
                trending-score: (calculate-trending-score 
                              (+ (get proposal-count existing-trend) u1)
                              (+ (get total-funding existing-trend) funding-amount)
                              (get subscriber-count existing-trend))
            }
        )
        
        (ok true)
    )
)

(define-private (calculate-trending-score (proposals uint) (funding uint) (subscribers uint))
    ;; Simple trending algorithm: weight proposals and funding with subscriber multiplier
    (* (+ (* proposals u10) (/ funding u100)) (if (> subscribers u0) subscribers u1))
)

;; Subscription Management Functions
(define-public (deactivate-subscription (subscription-id uint))
    (let ((user tx-sender)
          (subscription (unwrap! (map-get? user-subscriptions {user: user, subscription-id: subscription-id})
                                ERR-SUBSCRIPTION-NOT-FOUND)))
        
        (map-set user-subscriptions
            {user: user, subscription-id: subscription-id}
            (merge subscription {active: false})
        )
        
        (ok true)
    )
)

(define-public (reactivate-subscription (subscription-id uint))
    (let ((user tx-sender)
          (subscription (unwrap! (map-get? user-subscriptions {user: user, subscription-id: subscription-id})
                                ERR-SUBSCRIPTION-NOT-FOUND)))
        
        (map-set user-subscriptions
            {user: user, subscription-id: subscription-id}
            (merge subscription {active: true})
        )
        
        (ok true)
    )
)

;; Read-Only Functions
(define-read-only (get-user-subscriptions (user principal))
    ;; Returns subscription count - full iteration would require off-chain indexing
    (map-get? subscription-counts {user: user})
)

(define-read-only (get-subscription-details (user principal) (subscription-id uint))
    (map-get? user-subscriptions {user: user, subscription-id: subscription-id})
)

(define-read-only (get-user-alerts (user principal))
    (map-get? alert-counts {user: user})
)

(define-read-only (get-alert-details (user principal) (proposal-id uint) (alert-id uint))
    (map-get? proposal-alerts {user: user, proposal-id: proposal-id, alert-id: alert-id})
)

(define-read-only (get-field-subscription (field-id uint) (subscriber principal))
    (map-get? field-subscriptions {field-id: field-id, subscriber: subscriber})
)

(define-read-only (get-keyword-subscription (keyword (string-ascii 30)) (subscriber principal))
    (map-get? keyword-subscriptions {keyword: keyword, subscriber: subscriber})
)

(define-read-only (is-following-researcher (follower principal) (researcher principal))
    (map-get? author-followings {follower: follower, following: researcher})
)

(define-read-only (get-trending-topics (time-period uint))
    ;; Simplified - would return top trending topics for period
    (ok "Top trending topics would be listed here based on trending scores")
)

(define-read-only (get-saved-search (user principal) (search-id uint))
    (map-get? saved-searches {user: user, search-id: search-id})
)

(define-read-only (get-notification-preferences (user principal))
    (map-get? notification-preferences {user: user})
)

(define-read-only (calculate-relevance-score (user principal) (proposal-id uint))
    ;; Simplified relevance calculation based on user's subscriptions and interests
    ;; Would integrate with user's subscription preferences, voting history, etc.
    (ok u50) ;; Placeholder score
)

;; Admin Functions
(define-public (set-max-subscriptions (new-max uint))
    (begin
        (var-set max-subscriptions-per-user new-max)
        (ok true)
    )
)

(define-public (toggle-global-notifications (enabled bool))
    (begin
        (var-set global-notification-enabled enabled)
        (ok true)
    )
)

(define-public (cleanup-old-alerts (cutoff-block uint))
    ;; Simplified cleanup - would remove alerts older than cutoff block
    ;; In real implementation, would iterate through alerts
    (ok true)
)
