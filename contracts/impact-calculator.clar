;; Environmental Impact Calculator
;; Quantifies real-world environmental benefits from recycling activities

(define-constant ERR-NOT-AUTHORIZED (err u800))
(define-constant ERR-INVALID-MATERIAL (err u801))
(define-constant ERR-INVALID-AMOUNT (err u802))
(define-constant ERR-IMPACT-NOT-FOUND (err u803))

(define-data-var contract-admin principal tx-sender)
(define-data-var impact-report-counter uint u0)

;; Environmental impact factors for different materials
(define-map material-impact-factors
    { material-id: uint }
    {
        co2-saved-per-kg: uint,        ;; grams of CO2 equivalent saved
        energy-saved-per-kg: uint,     ;; joules of energy saved
        water-saved-per-kg: uint,      ;; milliliters of water saved
        landfill-diverted-per-kg: uint ;; grams diverted from landfill
    }
)

;; User cumulative environmental impact
(define-map user-environmental-impact
    { user: principal }
    {
        total-co2-saved: uint,
        total-energy-saved: uint,
        total-water-saved: uint,
        total-landfill-diverted: uint,
        impact-score: uint,
        last-updated: uint,
        milestone-level: uint
    }
)

;; Global platform impact tracking
(define-map global-impact-metrics
    { period: (string-ascii 20) }
    {
        total-users-active: uint,
        total-co2-saved: uint,
        total-energy-saved: uint,
        total-water-saved: uint,
        total-landfill-diverted: uint,
        period-start: uint,
        period-end: uint
    }
)

;; Impact milestones for user progression
(define-map impact-milestones
    { milestone-id: uint }
    {
        name: (string-ascii 50),
        co2-threshold: uint,
        energy-threshold: uint,
        reward-multiplier: uint,
        badge-unlock: uint
    }
)

;; Impact verification records
(define-map impact-reports
    { report-id: uint }
    {
        user: principal,
        material-recycled: uint,
        weight-kg: uint,
        environmental-benefit: {
            co2-saved: uint,
            energy-saved: uint,
            water-saved: uint,
            landfill-diverted: uint
        },
        verified-at: uint,
        verifier: principal
    }
)

;; Initialize default material impact factors
(define-public (initialize-impact-factors)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
        ;; Plastic (material-id: 1)
        (try! (set-material-impact-factor u1 u1800 u54000 u2000 u1000))
        ;; Paper (material-id: 2) 
        (try! (set-material-impact-factor u2 u900 u28000 u15000 u1000))
        ;; Glass (material-id: 3)
        (try! (set-material-impact-factor u3 u314 u7000 u1200 u1000))
        ;; Metal/Aluminum (material-id: 4)
        (try! (set-material-impact-factor u4 u9000 u200000 u1500 u1000))
        ;; Cardboard (material-id: 5)
        (try! (set-material-impact-factor u5 u700 u20000 u12000 u1000))
        (ok true)
    )
)

;; Set or update environmental impact factors for materials
(define-public (set-material-impact-factor 
    (material-id uint) 
    (co2-saved uint) 
    (energy-saved uint) 
    (water-saved uint) 
    (landfill-diverted uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
        (map-set material-impact-factors
            { material-id: material-id }
            {
                co2-saved-per-kg: co2-saved,
                energy-saved-per-kg: energy-saved,
                water-saved-per-kg: water-saved,
                landfill-diverted-per-kg: landfill-diverted
            })
        (ok true)
    )
)

;; Calculate and record environmental impact of recycling activity
(define-public (record-environmental-impact 
    (user principal) 
    (material-id uint) 
    (weight-kg uint))
    (let (
        (impact-factors (unwrap! (map-get? material-impact-factors { material-id: material-id }) ERR-INVALID-MATERIAL))
        (current-impact (default-to 
            {total-co2-saved: u0, total-energy-saved: u0, total-water-saved: u0, 
             total-landfill-diverted: u0, impact-score: u0, last-updated: u0, milestone-level: u0}
            (map-get? user-environmental-impact { user: user })))
        (co2-benefit (* weight-kg (get co2-saved-per-kg impact-factors)))
        (energy-benefit (* weight-kg (get energy-saved-per-kg impact-factors)))
        (water-benefit (* weight-kg (get water-saved-per-kg impact-factors)))
        (landfill-benefit (* weight-kg (get landfill-diverted-per-kg impact-factors)))
        (impact-score (calculate-impact-score co2-benefit energy-benefit water-benefit landfill-benefit))
        (report-id (+ (var-get impact-report-counter) u1))
    )
        (asserts! (> weight-kg u0) ERR-INVALID-AMOUNT)
        
        ;; Update user's cumulative impact
        (map-set user-environmental-impact
            { user: user }
            {
                total-co2-saved: (+ (get total-co2-saved current-impact) co2-benefit),
                total-energy-saved: (+ (get total-energy-saved current-impact) energy-benefit),
                total-water-saved: (+ (get total-water-saved current-impact) water-benefit),
                total-landfill-diverted: (+ (get total-landfill-diverted current-impact) landfill-benefit),
                impact-score: (+ (get impact-score current-impact) impact-score),
                last-updated: stacks-block-height,
                milestone-level: (get milestone-level current-impact)
            })
        
        ;; Create impact report record
        (var-set impact-report-counter report-id)
        (map-set impact-reports
            { report-id: report-id }
            {
                user: user,
                material-recycled: material-id,
                weight-kg: weight-kg,
                environmental-benefit: {
                    co2-saved: co2-benefit,
                    energy-saved: energy-benefit,
                    water-saved: water-benefit,
                    landfill-diverted: landfill-benefit
                },
                verified-at: stacks-block-height,
                verifier: tx-sender
            })
        
        ;; Check for milestone achievements (ignoring result)
        (unwrap-panic (check-milestone-progression user))
        
        (ok {
            co2-saved: co2-benefit,
            energy-saved: energy-benefit,
            water-saved: water-benefit,
            landfill-diverted: landfill-benefit,
            impact-score: impact-score,
            report-id: report-id
        })
    )
)

;; Calculate composite impact score
(define-private (calculate-impact-score (co2 uint) (energy uint) (water uint) (landfill uint))
    (+ (+ (/ co2 u100) (/ energy u10000)) (+ (/ water u1000) (/ landfill u100)))
)

;; Check and update milestone progression
(define-public (check-milestone-progression (user principal))
    (match (map-get? user-environmental-impact { user: user })
        user-impact (let (
            (co2-total (get total-co2-saved user-impact))
            (energy-total (get total-energy-saved user-impact))
            (current-level (get milestone-level user-impact))
            (new-level (determine-milestone-level co2-total energy-total))
        )
            (if (> new-level current-level)
                (begin
                    (map-set user-environmental-impact
                        { user: user }
                        (merge user-impact { milestone-level: new-level }))
                    (ok new-level)
                )
                (ok current-level)
            )
        )
        (ok u0)
    )
)

;; Determine milestone level based on cumulative impact
(define-private (determine-milestone-level (co2-saved uint) (energy-saved uint))
    (if (and (>= co2-saved u100000) (>= energy-saved u5000000))
        u5  ;; Sustainability Champion
        (if (and (>= co2-saved u50000) (>= energy-saved u2000000))
            u4  ;; Eco Leader
            (if (and (>= co2-saved u20000) (>= energy-saved u800000))
                u3  ;; Impact Maker
                (if (and (>= co2-saved u5000) (>= energy-saved u200000))
                    u2  ;; Green Contributor
                    (if (and (>= co2-saved u1000) (>= energy-saved u50000))
                        u1  ;; Eco Starter
                        u0  ;; No milestone
                    )
                )
            )
        )
    )
)

;; Update global impact metrics for reporting
(define-public (update-global-metrics (period (string-ascii 20)))
    (let (
        (current-metrics (default-to 
            {total-users-active: u0, total-co2-saved: u0, total-energy-saved: u0,
             total-water-saved: u0, total-landfill-diverted: u0, 
             period-start: stacks-block-height, period-end: stacks-block-height}
            (map-get? global-impact-metrics { period: period })))
    )
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
        (map-set global-impact-metrics
            { period: period }
            (merge current-metrics {
                total-users-active: (+ (get total-users-active current-metrics) u1),
                period-end: stacks-block-height
            })
        )
        (ok true)
    )
)

;; Create custom impact milestone
(define-public (create-impact-milestone 
    (milestone-id uint)
    (name (string-ascii 50))
    (co2-threshold uint)
    (energy-threshold uint)
    (reward-multiplier uint)
    (badge-unlock uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
        (map-set impact-milestones
            { milestone-id: milestone-id }
            {
                name: name,
                co2-threshold: co2-threshold,
                energy-threshold: energy-threshold,
                reward-multiplier: reward-multiplier,
                badge-unlock: badge-unlock
            })
        (ok true)
    )
)

;; Read-only functions

(define-read-only (get-user-environmental-impact (user principal))
    (map-get? user-environmental-impact { user: user })
)

(define-read-only (get-material-impact-factors (material-id uint))
    (map-get? material-impact-factors { material-id: material-id })
)

(define-read-only (get-impact-report (report-id uint))
    (map-get? impact-reports { report-id: report-id })
)

(define-read-only (get-global-impact-metrics (period (string-ascii 20)))
    (map-get? global-impact-metrics { period: period })
)

(define-read-only (get-impact-milestone (milestone-id uint))
    (map-get? impact-milestones { milestone-id: milestone-id })
)

;; Calculate potential impact for given weight and material
(define-read-only (calculate-potential-impact (material-id uint) (weight-kg uint))
    (match (map-get? material-impact-factors { material-id: material-id })
        factors (ok {
            co2-saved: (* weight-kg (get co2-saved-per-kg factors)),
            energy-saved: (* weight-kg (get energy-saved-per-kg factors)),
            water-saved: (* weight-kg (get water-saved-per-kg factors)),
            landfill-diverted: (* weight-kg (get landfill-diverted-per-kg factors))
        })
        ERR-INVALID-MATERIAL
    )
)

;; Get user's environmental profile summary
(define-read-only (get-user-impact-summary (user principal))
    (match (map-get? user-environmental-impact { user: user })
        impact-data (ok {
            total-impact-score: (get impact-score impact-data),
            current-milestone: (get milestone-level impact-data),
            co2-equivalent: (/ (get total-co2-saved impact-data) u1000), ;; Convert to kg
            energy-equivalent-hours: (/ (get total-energy-saved impact-data) u3600000), ;; Convert to kWh
            trees-equivalent: (/ (get total-co2-saved impact-data) u21000), ;; ~21kg CO2 per tree/year
            last-activity: (get last-updated impact-data)
        })
        (ok {
            total-impact-score: u0,
            current-milestone: u0,
            co2-equivalent: u0,
            energy-equivalent-hours: u0,
            trees-equivalent: u0,
            last-activity: u0
        })
    )
)

;; Administrative functions
(define-public (set-contract-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
        (var-set contract-admin new-admin)
        (ok true)
    )
)

(define-read-only (get-contract-admin)
    (var-get contract-admin)
)

(define-read-only (get-total-reports)
    (var-get impact-report-counter)
)
