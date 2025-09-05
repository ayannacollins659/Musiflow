;; Revenue Analytics Dashboard for Musiflow
;; Provides on-chain analytics for artist revenue streams and fan engagement

(define-constant BPS u10000)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_CONFIGURATION (err u400))

;; Only the allowed contract (main Musiflow contract) can record data
(define-data-var allowed-contract (optional principal) none)

;; Maps for storing monthly data
(define-map monthly-revenue 
  { artist: principal, month-id: uint } 
  { amount: uint }
)

(define-map fan-activity 
  { artist: principal, month-id: uint } 
  { plays: uint, likes: uint, purchases: uint }
)

;; Authorization functions
(define-public (set-allowed-contract (contract principal))
  (begin
    (asserts! (is-none (var-get allowed-contract)) ERR_CONFIGURATION)
    (var-set allowed-contract (some contract))
    (ok true)
  )
)

(define-public (update-allowed-contract (new-contract principal))
  (let ((current (var-get allowed-contract)))
    (asserts! (is-some current) ERR_CONFIGURATION)
    (asserts! (is-eq contract-caller (unwrap-panic current)) ERR_UNAUTHORIZED)
    (var-set allowed-contract (some new-contract))
    (ok true)
  )
)

;; Private helper functions
(define-read-only (get-revenue (artist principal) (month-id uint))
  (default-to u0 (get amount (map-get? monthly-revenue { artist: artist, month-id: month-id })))
)

(define-read-only (get-activity-data (artist principal) (month-id uint))
  (match (map-get? fan-activity { artist: artist, month-id: month-id })
    activity activity
    { plays: u0, likes: u0, purchases: u0 }
  )
)

;; Data recording functions (only callable by authorized contract)
(define-public (record-revenue (artist principal) (month-id uint) (amount uint))
  (begin
    (asserts! (is-eq (some contract-caller) (var-get allowed-contract)) ERR_UNAUTHORIZED)
    (let ((current-revenue (get-revenue artist month-id)))
      (map-set monthly-revenue 
        { artist: artist, month-id: month-id } 
        { amount: (+ current-revenue amount) }
      )
      (ok true)
    )
  )
)

(define-public (record-fan-activity (artist principal) (month-id uint) (plays uint) (likes uint) (purchases uint))
  (begin
    (asserts! (is-eq (some contract-caller) (var-get allowed-contract)) ERR_UNAUTHORIZED)
    (let ((current (get-activity-data artist month-id)))
      (map-set fan-activity 
        { artist: artist, month-id: month-id }
        { 
          plays: (+ (get plays current) plays), 
          likes: (+ (get likes current) likes), 
          purchases: (+ (get purchases current) purchases) 
        }
      )
      (ok true)
    )
  )
)

;; Public read functions for dashboard
(define-read-only (get-monthly-revenue (artist principal) (month-id uint))
  (get-revenue artist month-id)
)

(define-read-only (get-monthly-activity (artist principal) (month-id uint))
  (get-activity-data artist month-id)
)

(define-read-only (get-engagement-metrics (artist principal) (month-id uint))
  (let (
    (activity (get-activity-data artist month-id))
    (plays (get plays activity))
    (safe-plays (if (> plays u0) plays u1))
  )
    {
      plays: plays,
      likes: (get likes activity),
      purchases: (get purchases activity),
      like-rate-bps: (/ (* (get likes activity) BPS) safe-plays),
      conversion-rate-bps: (/ (* (get purchases activity) BPS) safe-plays)
    }
  )
)

(define-read-only (calculate-growth-rate (artist principal) (month1 uint) (month2 uint))
  (let (
    (revenue1 (get-revenue artist month1))
    (revenue2 (get-revenue artist month2))
    (base-revenue (if (> revenue1 u0) revenue1 u1))
    (growth-direction (if (> revenue2 revenue1) 1 (if (< revenue2 revenue1) -1 0)))
    (revenue-diff (- (to-int revenue2) (to-int revenue1)))
    (absolute-diff (if (>= revenue-diff 0) revenue-diff (- 0 revenue-diff)))
    (base-int (to-int base-revenue))
  )
    {
      direction: growth-direction,
      rate-bps: (/ (* absolute-diff 10000) base-int)
    }
  )
)

(define-read-only (get-revenue-forecast (artist principal) (month1 uint) (month2 uint) (month3 uint))
  (let ((total-revenue (+ (+ (get-revenue artist month1) (get-revenue artist month2)) (get-revenue artist month3))))
    (/ total-revenue u3)
  )
)

(define-read-only (get-quarterly-trend (artist principal) (month1 uint) (month2 uint) (month3 uint))
  (let (
    (rev1 (get-revenue artist month1))
    (rev2 (get-revenue artist month2))
    (rev3 (get-revenue artist month3))
    (growth12 (calculate-growth-rate artist month1 month2))
    (growth23 (calculate-growth-rate artist month2 month3))
  )
    {
      month1-revenue: rev1,
      month2-revenue: rev2,
      month3-revenue: rev3,
      growth12-direction: (get direction growth12),
      growth12-rate-bps: (get rate-bps growth12),
      growth23-direction: (get direction growth23),
      growth23-rate-bps: (get rate-bps growth23)
    }
  )
)

(define-read-only (get-comprehensive-report (artist principal) (month1 uint) (month2 uint) (month3 uint))
  (let (
    (rev1 (get-revenue artist month1)) 
    (rev2 (get-revenue artist month2)) 
    (rev3 (get-revenue artist month3))
    (total-revenue (+ (+ rev1 rev2) rev3))
    (average-revenue (/ total-revenue u3))
    (act1 (get-activity-data artist month1)) 
    (act2 (get-activity-data artist month2)) 
    (act3 (get-activity-data artist month3))
    (total-plays (+ (+ (get plays act1) (get plays act2)) (get plays act3)))
    (total-likes (+ (+ (get likes act1) (get likes act2)) (get likes act3)))
    (total-purchases (+ (+ (get purchases act1) (get purchases act2)) (get purchases act3)))
    (safe-plays (if (> total-plays u0) total-plays u1))
    (latest-growth (calculate-growth-rate artist month2 month3))
    (forecast (get-revenue-forecast artist month1 month2 month3))
  )
    {
      total-revenue: total-revenue,
      average-revenue: average-revenue,
      growth-direction: (get direction latest-growth),
      growth-rate-bps: (get rate-bps latest-growth),
      total-plays: total-plays,
      total-likes: total-likes,
      total-purchases: total-purchases,
      engagement-rate-bps: (/ (* total-likes BPS) safe-plays),
      conversion-rate-bps: (/ (* total-purchases BPS) safe-plays),
      next-month-forecast: forecast
    }
  )
)

;; Utility function
(define-read-only (get-allowed-contract)
  (var-get allowed-contract)
)
