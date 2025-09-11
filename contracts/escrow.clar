(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-INVALID-AMOUNT (err u301))
(define-constant ERR-INSUFFICIENT-BALANCE (err u302))
(define-constant ERR-ESCROW-NOT-FOUND (err u303))
(define-constant ERR-ESCROW-EXPIRED (err u304))
(define-constant ERR-INVALID-STATE (err u305))
(define-constant ERR-DISPUTE-PERIOD-ACTIVE (err u306))
(define-constant ERR-ALREADY-RELEASED (err u307))

(define-data-var escrow-counter uint u0)
(define-data-var arbitrator-address principal tx-sender)
(define-data-var escrow-fee-rate uint u100)
(define-data-var dispute-period-blocks uint u144)

(define-map escrow-agreements
  { escrow-id: uint }
  { buyer: principal,
    seller: principal,
    amount: uint,
    arbitrator-fee: uint,
    created-at: uint,
    expires-at: uint,
    state: uint,
    delivery-confirmed: bool,
    dispute-raised: bool,
    dispute-deadline: uint }
)

(define-map escrow-metadata
  { escrow-id: uint }
  { listing-id: uint,
    material-description: (string-ascii 100),
    delivery-terms: (string-ascii 200) }
)

(define-map dispute-records
  { escrow-id: uint }
  { dispute-reason: (string-ascii 300),
    raised-at: uint,
    raised-by: principal,
    resolution: (optional (string-ascii 200)),
    resolved-at: (optional uint) }
)

(define-public (create-escrow (seller principal) (amount uint) (duration-blocks uint) (listing-id uint) (material-description (string-ascii 100)) (delivery-terms (string-ascii 200)))
  (let
    (
      (escrow-id (+ (var-get escrow-counter) u1))
      (arbitrator-fee (/ (* amount (var-get escrow-fee-rate)) u10000))
      (total-amount (+ amount arbitrator-fee))
      (expires-at (+ stacks-block-height duration-blocks))
    )
    (begin
      (asserts! (> amount u0) ERR-INVALID-AMOUNT)
      (asserts! (> duration-blocks u0) ERR-INVALID-AMOUNT)
      (asserts! (not (is-eq tx-sender seller)) ERR-NOT-AUTHORIZED)
      (asserts! (>= (stx-get-balance tx-sender) total-amount) ERR-INSUFFICIENT-BALANCE)
      (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
      (var-set escrow-counter escrow-id)
      (map-set escrow-agreements
        { escrow-id: escrow-id }
        { buyer: tx-sender,
          seller: seller,
          amount: amount,
          arbitrator-fee: arbitrator-fee,
          created-at: stacks-block-height,
          expires-at: expires-at,
          state: u0,
          delivery-confirmed: false,
          dispute-raised: false,
          dispute-deadline: u0 })
      (map-set escrow-metadata
        { escrow-id: escrow-id }
        { listing-id: listing-id,
          material-description: material-description,
          delivery-terms: delivery-terms })
      (ok escrow-id)))
)

(define-public (confirm-delivery (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrow-agreements { escrow-id: escrow-id }) ERR-ESCROW-NOT-FOUND))
      (dispute-deadline (+ stacks-block-height (var-get dispute-period-blocks)))
    )
    (begin
      (asserts! (is-eq tx-sender (get seller escrow)) ERR-NOT-AUTHORIZED)
      (asserts! (is-eq (get state escrow) u0) ERR-INVALID-STATE)
      (asserts! (< stacks-block-height (get expires-at escrow)) ERR-ESCROW-EXPIRED)
      (map-set escrow-agreements
        { escrow-id: escrow-id }
        (merge escrow { delivery-confirmed: true, dispute-deadline: dispute-deadline }))
      (ok true)))
)

(define-public (release-funds (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrow-agreements { escrow-id: escrow-id }) ERR-ESCROW-NOT-FOUND))
      (seller (get seller escrow))
      (amount (get amount escrow))
      (arbitrator-fee (get arbitrator-fee escrow))
    )
    (begin
      (asserts! (get delivery-confirmed escrow) ERR-INVALID-STATE)
      (asserts! (> (get dispute-deadline escrow) u0) ERR-INVALID-STATE)
      (asserts! (> stacks-block-height (get dispute-deadline escrow)) ERR-DISPUTE-PERIOD-ACTIVE)
      (asserts! (not (get dispute-raised escrow)) ERR-DISPUTE-PERIOD-ACTIVE)
      (asserts! (is-eq (get state escrow) u0) ERR-ALREADY-RELEASED)
      (try! (as-contract (stx-transfer? amount tx-sender seller)))
      (try! (as-contract (stx-transfer? arbitrator-fee tx-sender (var-get arbitrator-address))))
      (map-set escrow-agreements
        { escrow-id: escrow-id }
        (merge escrow { state: u1 }))
      (ok true)))
)

(define-public (raise-dispute (escrow-id uint) (reason (string-ascii 300)))
  (let
    (
      (escrow (unwrap! (map-get? escrow-agreements { escrow-id: escrow-id }) ERR-ESCROW-NOT-FOUND))
      (buyer (get buyer escrow))
    )
    (begin
      (asserts! (is-eq tx-sender buyer) ERR-NOT-AUTHORIZED)
      (asserts! (get delivery-confirmed escrow) ERR-INVALID-STATE)
      (asserts! (< stacks-block-height (get dispute-deadline escrow)) ERR-ESCROW-EXPIRED)
      (asserts! (not (get dispute-raised escrow)) ERR-INVALID-STATE)
      (map-set escrow-agreements
        { escrow-id: escrow-id }
        (merge escrow { dispute-raised: true }))
      (map-set dispute-records
        { escrow-id: escrow-id }
        { dispute-reason: reason,
          raised-at: stacks-block-height,
          raised-by: tx-sender,
          resolution: none,
          resolved-at: none })
      (ok true)))
)

(define-public (resolve-dispute (escrow-id uint) (buyer-refund-percentage uint) (resolution (string-ascii 200)))
  (let
    (
      (escrow (unwrap! (map-get? escrow-agreements { escrow-id: escrow-id }) ERR-ESCROW-NOT-FOUND))
      (dispute (unwrap! (map-get? dispute-records { escrow-id: escrow-id }) ERR-ESCROW-NOT-FOUND))
      (total-amount (get amount escrow))
      (buyer-refund (/ (* total-amount buyer-refund-percentage) u100))
      (seller-payment (- total-amount buyer-refund))
      (arbitrator-fee (get arbitrator-fee escrow))
    )
    (begin
      (asserts! (is-eq tx-sender (var-get arbitrator-address)) ERR-NOT-AUTHORIZED)
      (asserts! (get dispute-raised escrow) ERR-INVALID-STATE)
      (asserts! (<= buyer-refund-percentage u100) ERR-INVALID-AMOUNT)
      (asserts! (is-eq (get state escrow) u0) ERR-ALREADY-RELEASED)
      (try! (as-contract (stx-transfer? arbitrator-fee tx-sender (var-get arbitrator-address))))
      (map-set escrow-agreements
        { escrow-id: escrow-id }
        (merge escrow { state: u2 }))
      (map-set dispute-records
        { escrow-id: escrow-id }
        (merge dispute { resolution: (some resolution), resolved-at: (some stacks-block-height) }))
      (ok true)))
)

(define-public (cancel-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrow-agreements { escrow-id: escrow-id }) ERR-ESCROW-NOT-FOUND))
      (buyer (get buyer escrow))
      (total-refund (+ (get amount escrow) (get arbitrator-fee escrow)))
    )
    (begin
      (asserts! (is-eq tx-sender buyer) ERR-NOT-AUTHORIZED)
      (asserts! (not (get delivery-confirmed escrow)) ERR-INVALID-STATE)
      (asserts! (> stacks-block-height (get expires-at escrow)) ERR-DISPUTE-PERIOD-ACTIVE)
      (asserts! (is-eq (get state escrow) u0) ERR-ALREADY-RELEASED)
      (try! (as-contract (stx-transfer? total-refund tx-sender buyer)))
      (map-set escrow-agreements
        { escrow-id: escrow-id }
        (merge escrow { state: u3 }))
      (ok true)))
)

(define-public (set-arbitrator (new-arbitrator principal))
  (begin
    (asserts! (is-eq tx-sender (var-get arbitrator-address)) ERR-NOT-AUTHORIZED)
    (ok (var-set arbitrator-address new-arbitrator)))
)

(define-public (update-escrow-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get arbitrator-address)) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-rate u1000) ERR-INVALID-AMOUNT)
    (ok (var-set escrow-fee-rate new-rate)))
)

(define-public (update-dispute-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender (var-get arbitrator-address)) ERR-NOT-AUTHORIZED)
    (asserts! (> new-period u0) ERR-INVALID-AMOUNT)
    (ok (var-set dispute-period-blocks new-period)))
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrow-agreements { escrow-id: escrow-id })
)

(define-read-only (get-escrow-metadata (escrow-id uint))
  (map-get? escrow-metadata { escrow-id: escrow-id })
)

(define-read-only (get-dispute-record (escrow-id uint))
  (map-get? dispute-records { escrow-id: escrow-id })
)

(define-read-only (get-total-escrows)
  (var-get escrow-counter)
)

(define-read-only (get-arbitrator)
  (var-get arbitrator-address)
)

(define-read-only (get-escrow-fee-rate)
  (var-get escrow-fee-rate)
)

(define-read-only (get-dispute-period)
  (var-get dispute-period-blocks)
)

(define-read-only (is-escrow-active (escrow-id uint))
  (match (map-get? escrow-agreements { escrow-id: escrow-id })
    escrow (and (is-eq (get state escrow) u0) (< stacks-block-height (get expires-at escrow)))
    false)
)

(define-read-only (can-release-funds (escrow-id uint))
  (match (map-get? escrow-agreements { escrow-id: escrow-id })
    escrow (and 
      (get delivery-confirmed escrow) 
      (> (get dispute-deadline escrow) u0)
      (> stacks-block-height (get dispute-deadline escrow))
      (not (get dispute-raised escrow))
      (is-eq (get state escrow) u0))
    false)
)

(define-read-only (get-escrow-balance (escrow-id uint))
  (match (map-get? escrow-agreements { escrow-id: escrow-id })
    escrow (if (is-eq (get state escrow) u0)
      (some (+ (get amount escrow) (get arbitrator-fee escrow)))
      (some u0))
    none)
)
