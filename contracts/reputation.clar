;; Reputation & Badge System for Recycling Platform
;; Gamifies recycling activities and builds marketplace trust

(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-INVALID-AMOUNT (err u401))
(define-constant ERR-BADGE-NOT-FOUND (err u402))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u403))
(define-constant ERR-BADGE-ALREADY-OWNED (err u404))
(define-constant ERR-INVALID-BADGE-TYPE (err u405))

;; Badge type constants
(define-constant BADGE-ROOKIE u1)
(define-constant BADGE-RECYCLER u2)
(define-constant BADGE-ECO-WARRIOR u3)
(define-constant BADGE-MARKETPLACE-HERO u4)
(define-constant BADGE-STREAK-MASTER u5)
(define-constant BADGE-VOLUME-CHAMPION u6)
(define-constant BADGE-TRUSTED-TRADER u7)
(define-constant BADGE-SUSTAINABILITY-LEGEND u8)

;; Reputation decay constants
(define-constant DECAY-PERIOD u4320) ;; 30 days in blocks (144 blocks/day)
(define-constant DECAY-RATE u5) ;; 5% decay per period

;; System configuration
(define-data-var reputation-admin principal tx-sender)
(define-data-var reputation-multiplier-rate uint u150) ;; 1.5x multiplier at max reputation
(define-data-var badge-counter uint u0)
(define-data-var min-reputation-threshold uint u100)

;; Core reputation tracking for each user
(define-map user-reputation
  { user: principal }
  { base-score: uint,
    bonus-score: uint,
    total-score: uint,
    last-activity: uint,
    last-decay: uint,
    recycling-points: uint,
    marketplace-points: uint,
    community-points: uint }
)

;; Badge definitions with requirements and rewards
(define-map badge-definitions
  { badge-id: uint }
  { name: (string-ascii 50),
    description: (string-ascii 200),
    reputation-requirement: uint,
    recycling-requirement: uint,
    marketplace-requirement: uint,
    reputation-reward: uint,
    is-active: bool }
)

;; User badge ownership tracking
(define-map user-badges
  { user: principal, badge-id: uint }
  { earned-at: uint,
    is-equipped: bool }
)

;; Badge showcase - users can display their favorite badges
(define-map user-showcase
  { user: principal }
  { primary-badge: (optional uint),
    secondary-badge: (optional uint),
    showcase-updated: uint }
)

;; Reputation activity log for transparency
(define-map reputation-history
  { user: principal, activity-id: uint }
  { activity-type: (string-ascii 30),
    points-earned: uint,
    timestamp: uint,
    description: (string-ascii 100) }
)

;; User activity counters for badge requirements
(define-map user-activity-stats
  { user: principal }
  { total-recycled-kg: uint,
    successful-trades: uint,
    community-contributions: uint,
    current-streak: uint,
    max-streak: uint }
)

;; Initialize default badge definitions
(define-public (initialize-badges)
  (begin
    (asserts! (is-eq tx-sender (var-get reputation-admin)) ERR-NOT-AUTHORIZED)
    (try! (create-badge BADGE-ROOKIE "Eco Rookie" "Started your recycling journey" u0 u10 u0 u50))
    (try! (create-badge BADGE-RECYCLER "Active Recycler" "Recycled 100kg of materials" u100 u100 u0 u100))
    (try! (create-badge BADGE-ECO-WARRIOR "Eco Warrior" "Recycled 500kg and earned high reputation" u500 u500 u10 u200))
    (try! (create-badge BADGE-MARKETPLACE-HERO "Marketplace Hero" "Completed 50 successful trades" u300 u200 u50 u150))
    (try! (create-badge BADGE-STREAK-MASTER "Streak Master" "Maintained 30-day recycling streak" u400 u300 u5 u250))
    (try! (create-badge BADGE-VOLUME-CHAMPION "Volume Champion" "Recycled over 1000kg of materials" u800 u1000 u20 u300))
    (try! (create-badge BADGE-TRUSTED-TRADER "Trusted Trader" "100 trades with perfect reputation" u1000 u500 u100 u400))
    (try! (create-badge BADGE-SUSTAINABILITY-LEGEND "Sustainability Legend" "Ultimate eco achievement" u2000 u2000 u200 u500))
    (ok true))
)

;; Create a new badge definition
(define-public (create-badge (badge-id uint) (name (string-ascii 50)) (description (string-ascii 200)) (rep-req uint) (recycling-req uint) (marketplace-req uint) (rep-reward uint))
  (begin
    (asserts! (is-eq tx-sender (var-get reputation-admin)) ERR-NOT-AUTHORIZED)
    (map-set badge-definitions
      { badge-id: badge-id }
      { name: name,
        description: description,
        reputation-requirement: rep-req,
        recycling-requirement: recycling-req,
        marketplace-requirement: marketplace-req,
        reputation-reward: rep-reward,
        is-active: true })
    (var-set badge-counter (if (> badge-id (var-get badge-counter)) badge-id (var-get badge-counter)))
    (ok badge-id))
)

;; Award reputation points for recycling activities
(define-public (award-recycling-reputation (user principal) (weight-kg uint) (quality-bonus uint))
  (let
    (
      (current-rep (default-to { base-score: u0, bonus-score: u0, total-score: u0, last-activity: u0, last-decay: u0, recycling-points: u0, marketplace-points: u0, community-points: u0 }
        (map-get? user-reputation { user: user })))
      (base-points (* weight-kg u10)) ;; 10 points per kg
      (total-points (+ base-points quality-bonus))
      (new-recycling-points (+ (get recycling-points current-rep) total-points))
      (new-total-score (+ (get total-score current-rep) total-points))
    )
    (begin
      (asserts! (is-eq tx-sender (var-get reputation-admin)) ERR-NOT-AUTHORIZED)
      (map-set user-reputation
        { user: user }
        (merge current-rep { 
          base-score: (+ (get base-score current-rep) total-points),
          total-score: new-total-score,
          last-activity: stacks-block-height,
          recycling-points: new-recycling-points }))
      (unwrap-panic (log-reputation-activity user total-points "recycling" "Recycling activity points"))
      (unwrap-panic (update-activity-stats user weight-kg u0 u0))
      (unwrap-panic (check-and-award-badges user))
      (ok total-points)))
)

;; Award reputation for marketplace activities
(define-public (award-marketplace-reputation (user principal) (trade-value uint) (feedback-score uint))
  (let
    (
      (current-rep (default-to { base-score: u0, bonus-score: u0, total-score: u0, last-activity: u0, last-decay: u0, recycling-points: u0, marketplace-points: u0, community-points: u0 }
        (map-get? user-reputation { user: user })))
      (base-points (/ trade-value u100)) ;; 1 point per 100 units traded
      (feedback-bonus (* feedback-score u5)) ;; 5 points per feedback point
      (total-points (+ base-points feedback-bonus))
      (new-marketplace-points (+ (get marketplace-points current-rep) total-points))
      (new-total-score (+ (get total-score current-rep) total-points))
    )
    (begin
      (asserts! (is-eq tx-sender (var-get reputation-admin)) ERR-NOT-AUTHORIZED)
      (map-set user-reputation
        { user: user }
        (merge current-rep { 
          base-score: (+ (get base-score current-rep) total-points),
          total-score: new-total-score,
          last-activity: stacks-block-height,
          marketplace-points: new-marketplace-points }))
      (unwrap-panic (log-reputation-activity user total-points "marketplace" "Marketplace trade points"))
      (unwrap-panic (update-activity-stats user u0 u1 u0))
      (unwrap-panic (check-and-award-badges user))
      (ok total-points)))
)

;; Update user activity statistics
(define-public (update-activity-stats (user principal) (recycled-kg uint) (trades uint) (contributions uint))
  (let
    (
      (current-stats (default-to { total-recycled-kg: u0, successful-trades: u0, community-contributions: u0, current-streak: u0, max-streak: u0 }
        (map-get? user-activity-stats { user: user })))
    )
    (begin
      (map-set user-activity-stats
        { user: user }
        { total-recycled-kg: (+ (get total-recycled-kg current-stats) recycled-kg),
          successful-trades: (+ (get successful-trades current-stats) trades),
          community-contributions: (+ (get community-contributions current-stats) contributions),
          current-streak: (get current-streak current-stats), ;; Managed separately
          max-streak: (get max-streak current-stats) })
      (ok true)))
)

;; Check and automatically award badges based on user achievements
(define-public (check-and-award-badges (user principal))
  (let
    (
      (user-rep (unwrap! (map-get? user-reputation { user: user }) (ok false)))
      (user-stats (default-to { total-recycled-kg: u0, successful-trades: u0, community-contributions: u0, current-streak: u0, max-streak: u0 }
        (map-get? user-activity-stats { user: user })))
    )
    (begin
      ;; Check each badge type and award if eligible
      (unwrap-panic (award-badge-if-eligible user BADGE-ROOKIE user-rep user-stats))
      (unwrap-panic (award-badge-if-eligible user BADGE-RECYCLER user-rep user-stats))
      (unwrap-panic (award-badge-if-eligible user BADGE-ECO-WARRIOR user-rep user-stats))
      (unwrap-panic (award-badge-if-eligible user BADGE-MARKETPLACE-HERO user-rep user-stats))
      (unwrap-panic (award-badge-if-eligible user BADGE-VOLUME-CHAMPION user-rep user-stats))
      (unwrap-panic (award-badge-if-eligible user BADGE-TRUSTED-TRADER user-rep user-stats))
      (ok true)))
)

;; Helper function to award badge if user meets requirements
(define-private (award-badge-if-eligible (user principal) (badge-id uint) (user-rep (tuple (base-score uint) (bonus-score uint) (total-score uint) (last-activity uint) (last-decay uint) (recycling-points uint) (marketplace-points uint) (community-points uint))) (user-stats (tuple (total-recycled-kg uint) (successful-trades uint) (community-contributions uint) (current-streak uint) (max-streak uint))))
  (match (map-get? badge-definitions { badge-id: badge-id })
    badge-def (if (and 
                   (>= (get total-score user-rep) (get reputation-requirement badge-def))
                   (>= (get total-recycled-kg user-stats) (get recycling-requirement badge-def))
                   (>= (get successful-trades user-stats) (get marketplace-requirement badge-def))
                   (is-none (map-get? user-badges { user: user, badge-id: badge-id })))
                (begin
                  (map-set user-badges
                    { user: user, badge-id: badge-id }
                    { earned-at: stacks-block-height, is-equipped: false })
                  ;; Award reputation bonus for earning badge
                  (let ((bonus-points (get reputation-reward badge-def)))
                    (map-set user-reputation
                      { user: user }
                      (merge user-rep { 
                        bonus-score: (+ (get bonus-score user-rep) bonus-points),
                        total-score: (+ (get total-score user-rep) bonus-points) }))
                    (unwrap-panic (log-reputation-activity user bonus-points "badge" "Badge earned bonus")))
                  (ok true))
                (ok false))
    (ok false))
)

;; Apply reputation decay over time to encourage continued activity
(define-public (apply-reputation-decay (user principal))
  (let
    (
      (current-rep (unwrap! (map-get? user-reputation { user: user }) ERR-INVALID-AMOUNT))
      (blocks-since-decay (- stacks-block-height (get last-decay current-rep)))
      (decay-periods (/ blocks-since-decay DECAY-PERIOD))
    )
    (begin
      (asserts! (> decay-periods u0) ERR-INVALID-AMOUNT)
      (let
        (
          (decay-amount (/ (* (get base-score current-rep) DECAY-RATE decay-periods) u100))
          (new-base-score (if (> decay-amount (get base-score current-rep))
                            u0
                            (- (get base-score current-rep) decay-amount)))
          (new-total-score (+ new-base-score (get bonus-score current-rep)))
        )
        (map-set user-reputation
          { user: user }
          (merge current-rep { 
            base-score: new-base-score,
            total-score: new-total-score,
            last-decay: stacks-block-height }))
        (ok decay-amount))))
)

;; User activity ID tracking
(define-map user-activity-counters
  { user: principal }
  { last-activity-id: uint }
)

;; Log reputation activity for transparency
(define-private (log-reputation-activity (user principal) (points uint) (activity-type (string-ascii 30)) (description (string-ascii 100)))
  (let
    (
      (current-counter (default-to { last-activity-id: u0 } (map-get? user-activity-counters { user: user })))
      (new-activity-id (+ (get last-activity-id current-counter) u1))
    )
    (begin
      (map-set user-activity-counters
        { user: user }
        { last-activity-id: new-activity-id })
      (map-set reputation-history
        { user: user, activity-id: new-activity-id }
        { activity-type: activity-type,
          points-earned: points,
          timestamp: stacks-block-height,
          description: description })
      (ok new-activity-id)))
)

;; Equip a badge for display
(define-public (equip-badge (badge-id uint) (is-primary bool))
  (let
    (
      (current-showcase (default-to { primary-badge: none, secondary-badge: none, showcase-updated: u0 }
        (map-get? user-showcase { user: tx-sender })))
    )
    (begin
      (asserts! (is-some (map-get? user-badges { user: tx-sender, badge-id: badge-id })) ERR-BADGE-NOT-FOUND)
      (if is-primary
        (map-set user-showcase
          { user: tx-sender }
          (merge current-showcase { primary-badge: (some badge-id), showcase-updated: stacks-block-height }))
        (map-set user-showcase
          { user: tx-sender }
          (merge current-showcase { secondary-badge: (some badge-id), showcase-updated: stacks-block-height })))
      (ok true)))
)

;; Get user's complete reputation profile
(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation { user: user })
)

;; Get user's badge collection
(define-read-only (get-user-badge (user principal) (badge-id uint))
  (map-get? user-badges { user: user, badge-id: badge-id })
)

;; Get badge definition
(define-read-only (get-badge-definition (badge-id uint))
  (map-get? badge-definitions { badge-id: badge-id })
)

;; Get user's showcase
(define-read-only (get-user-showcase (user principal))
  (map-get? user-showcase { user: user })
)

;; Get reputation-based multiplier for rewards
(define-read-only (get-reputation-multiplier (user principal))
  (match (map-get? user-reputation { user: user })
    rep-data (let ((score (get total-score rep-data)))
               (if (>= score u1000)
                 (var-get reputation-multiplier-rate)
                 (+ u100 (/ (* score (- (var-get reputation-multiplier-rate) u100)) u1000))))
    u100)
)

;; Check if user has specific badge
(define-read-only (has-badge (user principal) (badge-id uint))
  (is-some (map-get? user-badges { user: user, badge-id: badge-id }))
)

;; Get user activity statistics
(define-read-only (get-user-activity-stats (user principal))
  (map-get? user-activity-stats { user: user })
)

;; Administrative functions
(define-public (set-reputation-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get reputation-admin)) ERR-NOT-AUTHORIZED)
    (ok (var-set reputation-admin new-admin)))
)

(define-public (update-multiplier-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get reputation-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= new-rate u100) (<= new-rate u300)) ERR-INVALID-AMOUNT)
    (ok (var-set reputation-multiplier-rate new-rate)))
)

(define-read-only (get-reputation-admin)
  (var-get reputation-admin)
)

(define-read-only (get-multiplier-rate)
  (var-get reputation-multiplier-rate)
)


