
;; title: recycling
;; version:
;; summary:
;; description:

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-MATERIAL (err u103))

(define-fungible-token recycling-token)

(define-data-var authorized-verifier principal tx-sender)

(define-map recycler-stats
  { recycler: principal }
  { total-recycled: uint,
    total-rewards: uint,
    last-recycled: uint }
)

(define-map material-rates
  { material-id: uint }
  { tokens-per-kg: uint }
)

(define-public (initialize-material-rate (material-id uint) (tokens-per-kg uint))
  (begin
    (asserts! (is-eq tx-sender (var-get authorized-verifier)) ERR-NOT-AUTHORIZED)
    (ok (map-set material-rates { material-id: material-id } { tokens-per-kg: tokens-per-kg }))
  )
)

(define-public (set-verifier (new-verifier principal))
  (begin
    (asserts! (is-eq tx-sender (var-get authorized-verifier)) ERR-NOT-AUTHORIZED)
    (ok (var-set authorized-verifier new-verifier))
  )
)

(define-public (record-recycling (recycler principal) (material-id uint) (weight uint))
  (let 
    (
      (rate (unwrap! (get-material-rate material-id) ERR-INVALID-MATERIAL))
      (reward (* weight rate))
      (stats (default-to { total-recycled: u0, total-rewards: u0, last-recycled: u0 } 
        (map-get? recycler-stats { recycler: recycler })))
    )
    (asserts! (is-eq tx-sender (var-get authorized-verifier)) ERR-NOT-AUTHORIZED)
    (asserts! (> weight u0) ERR-INVALID-AMOUNT)
    (try! (ft-mint? recycling-token reward recycler))
    (ok (map-set recycler-stats
      { recycler: recycler }
      { total-recycled: (+ (get total-recycled stats) weight),
        total-rewards: (+ (get total-rewards stats) reward),
        last-recycled: stacks-block-height }))
  )
)

(define-public (transfer-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (ft-transfer? recycling-token amount tx-sender recipient)
  )
)

(define-read-only (get-material-rate (material-id uint))
  (get tokens-per-kg (map-get? material-rates { material-id: material-id }))
)

(define-read-only (get-recycler-stats (recycler principal))
  (map-get? recycler-stats { recycler: recycler })
)

(define-read-only (get-balance (account principal))
  (ft-get-balance recycling-token account)
)

(define-read-only (get-verifier)
  (var-get authorized-verifier)
)

(define-read-only (get-token-name)
  (ok "Recycling Token")
)

(define-read-only (get-token-symbol)
  (ok "RCYL")
)

(define-read-only (get-token-decimals)
  (ok u6)
)

(define-read-only (get-token-uri)
  (ok none)
)


(define-map material-categories
  { category-id: uint }
  { name: (string-ascii 20),
    bonus-multiplier: uint }
)

(define-map material-category-mapping
  { material-id: uint }
  { category-id: uint }
)

(define-public (add-material-category (category-id uint) (name (string-ascii 20)) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender (var-get authorized-verifier)) ERR-NOT-AUTHORIZED)
    (ok (map-set material-categories 
      { category-id: category-id }
      { name: name, bonus-multiplier: multiplier }))
  )
)

(define-public (map-material-to-category (material-id uint) (category-id uint))
  (begin
    (asserts! (is-eq tx-sender (var-get authorized-verifier)) ERR-NOT-AUTHORIZED)
    (ok (map-set material-category-mapping
      { material-id: material-id }
      { category-id: category-id }))
  )
)



(define-constant STREAK-THRESHOLD u604800)
(define-constant STREAK-BONUS u10)

(define-map recycler-streaks
  { recycler: principal }
  { current-streak: uint,
    last-activity: uint,
    total-streak-bonuses: uint }
)

(define-public (process-streak (recycler principal))
  (let
    (
      (current-time stacks-block-height)
      (streak-data (default-to { current-streak: u0, last-activity: u0, total-streak-bonuses: u0 }
        (map-get? recycler-streaks { recycler: recycler })))
      (time-diff (- current-time (get last-activity streak-data)))
      (new-streak (if (< time-diff STREAK-THRESHOLD)
        (+ (get current-streak streak-data) u1)
        u1))
      (bonus-amount (* STREAK-BONUS new-streak))
    )
    (begin
      (try! (ft-mint? recycling-token bonus-amount recycler))
      (ok (map-set recycler-streaks
        { recycler: recycler }
        { current-streak: new-streak,
          last-activity: current-time,
          total-streak-bonuses: (+ (get total-streak-bonuses streak-data) bonus-amount) })))
  )
)