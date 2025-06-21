(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-INVALID-AMOUNT (err u201))
(define-constant ERR-INSUFFICIENT-BALANCE (err u202))
(define-constant ERR-LISTING-NOT-FOUND (err u203))
(define-constant ERR-CANNOT-BUY-OWN-LISTING (err u204))
(define-constant ERR-LISTING-EXPIRED (err u205))

(define-fungible-token marketplace-token)

(define-data-var listing-counter uint u0)
(define-data-var marketplace-fee-rate uint u250)
(define-data-var fee-collector principal tx-sender)

(define-map material-listings
  { listing-id: uint }
  { seller: principal,
    material-id: uint,
    quantity: uint,
    price-per-kg: uint,
    total-price: uint,
    created-at: uint,
    expires-at: uint,
    is-active: bool }
)

(define-map seller-listings
  { seller: principal, listing-id: uint }
  { active: bool }
)

(define-map marketplace-stats
  { user: principal }
  { total-sold: uint,
    total-bought: uint,
    total-earnings: uint,
    total-spent: uint,
    successful-trades: uint }
)

(define-public (create-listing (material-id uint) (quantity uint) (price-per-kg uint) (duration-blocks uint))
  (let
    (
      (listing-id (+ (var-get listing-counter) u1))
      (total-price (* quantity price-per-kg))
      (expires-at (+ stacks-block-height duration-blocks))
    )
    (begin
      (asserts! (> quantity u0) ERR-INVALID-AMOUNT)
      (asserts! (> price-per-kg u0) ERR-INVALID-AMOUNT)
      (asserts! (> duration-blocks u0) ERR-INVALID-AMOUNT)
      (var-set listing-counter listing-id)
      (map-set material-listings
        { listing-id: listing-id }
        { seller: tx-sender,
          material-id: material-id,
          quantity: quantity,
          price-per-kg: price-per-kg,
          total-price: total-price,
          created-at: stacks-block-height,
          expires-at: expires-at,
          is-active: true })
      (map-set seller-listings
        { seller: tx-sender, listing-id: listing-id }
        { active: true })
      (ok listing-id)))
)

(define-public (purchase-listing (listing-id uint))
  (let
    (
      (listing (unwrap! (map-get? material-listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
      (seller (get seller listing))
      (total-price (get total-price listing))
      (marketplace-fee (/ (* total-price (var-get marketplace-fee-rate)) u10000))
      (seller-amount (- total-price marketplace-fee))
      (buyer-stats (default-to { total-sold: u0, total-bought: u0, total-earnings: u0, total-spent: u0, successful-trades: u0 }
        (map-get? marketplace-stats { user: tx-sender })))
      (seller-stats (default-to { total-sold: u0, total-bought: u0, total-earnings: u0, total-spent: u0, successful-trades: u0 }
        (map-get? marketplace-stats { user: seller })))
    )
    (begin
      (asserts! (get is-active listing) ERR-LISTING-NOT-FOUND)
      (asserts! (not (is-eq tx-sender seller)) ERR-CANNOT-BUY-OWN-LISTING)
      (asserts! (< stacks-block-height (get expires-at listing)) ERR-LISTING-EXPIRED)
      (asserts! (>= (stx-get-balance tx-sender) total-price) ERR-INSUFFICIENT-BALANCE)
      (try! (stx-transfer? seller-amount tx-sender seller))
      (try! (stx-transfer? marketplace-fee tx-sender (var-get fee-collector)))
      (map-set material-listings
        { listing-id: listing-id }
        (merge listing { is-active: false }))
      (map-set seller-listings
        { seller: seller, listing-id: listing-id }
        { active: false })
      (map-set marketplace-stats
        { user: tx-sender }
        { total-sold: (get total-sold buyer-stats),
          total-bought: (+ (get total-bought buyer-stats) (get quantity listing)),
          total-earnings: (get total-earnings buyer-stats),
          total-spent: (+ (get total-spent buyer-stats) total-price),
          successful-trades: (+ (get successful-trades buyer-stats) u1) })
      (map-set marketplace-stats
        { user: seller }
        { total-sold: (+ (get total-sold seller-stats) (get quantity listing)),
          total-bought: (get total-bought seller-stats),
          total-earnings: (+ (get total-earnings seller-stats) seller-amount),
          total-spent: (get total-spent seller-stats),
          successful-trades: (+ (get successful-trades seller-stats) u1) })
      (ok true)))
)

(define-public (cancel-listing (listing-id uint))
  (let
    (
      (listing (unwrap! (map-get? material-listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
    )
    (begin
      (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
      (asserts! (get is-active listing) ERR-LISTING-NOT-FOUND)
      (map-set material-listings
        { listing-id: listing-id }
        (merge listing { is-active: false }))
      (map-set seller-listings
        { seller: tx-sender, listing-id: listing-id }
        { active: false })
      (ok true)))
)

(define-public (update-marketplace-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get fee-collector)) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee-rate u1000) ERR-INVALID-AMOUNT)
    (ok (var-set marketplace-fee-rate new-fee-rate)))
)

(define-public (set-fee-collector (new-collector principal))
  (begin
    (asserts! (is-eq tx-sender (var-get fee-collector)) ERR-NOT-AUTHORIZED)
    (ok (var-set fee-collector new-collector)))
)

(define-read-only (get-listing (listing-id uint))
  (map-get? material-listings { listing-id: listing-id })
)

(define-read-only (get-marketplace-stats (user principal))
  (map-get? marketplace-stats { user: user })
)

(define-read-only (get-marketplace-fee-rate)
  (var-get marketplace-fee-rate)
)

(define-read-only (get-fee-collector)
  (var-get fee-collector)
)

(define-read-only (get-total-listings)
  (var-get listing-counter)
)

(define-read-only (is-listing-active (listing-id uint))
  (match (map-get? material-listings { listing-id: listing-id })
    listing (and (get is-active listing) (< stacks-block-height (get expires-at listing)))
    false)
)

(define-read-only (get-listing-price-info (listing-id uint))
  (match (map-get? material-listings { listing-id: listing-id })
    listing (some { price-per-kg: (get price-per-kg listing),
                   total-price: (get total-price listing),
                   marketplace-fee: (/ (* (get total-price listing) (var-get marketplace-fee-rate)) u10000) })
    none)
)