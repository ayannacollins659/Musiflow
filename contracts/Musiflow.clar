(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_ALREADY_EXISTS (err u104))
(define-constant ERR_INVALID_PERCENTAGE (err u105))
(define-constant ERR_STREAM_INACTIVE (err u106))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u107))
(define-constant ERR_SUBSCRIPTION_EXPIRED (err u108))
(define-constant ERR_INVALID_DURATION (err u109))
(define-constant ERR_SUBSCRIPTION_ALREADY_EXISTS (err u110))
(define-constant ERR_INSUFFICIENT_POINTS (err u111))
(define-constant ERR_REWARD_NOT_FOUND (err u112))
(define-constant ERR_REWARD_UNAVAILABLE (err u113))
(define-constant ERR_INVALID_POINTS (err u114))

(define-data-var next-stream-id uint u1)
(define-data-var platform-fee-percentage uint u250)
(define-data-var next-subscription-id uint u1)
(define-data-var next-reward-id uint u1)

(define-map streams
  uint
  {
    artist: principal,
    title: (string-ascii 100),
    total-revenue: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map collaborators
  { stream-id: uint, collaborator: principal }
  { percentage: uint }
)

(define-map stream-collaborators
  uint
  { collaborator-list: (list 10 principal) }
)

(define-map revenue-deposits
  { stream-id: uint, depositor: principal }
  { amount: uint, timestamp: uint }
)

(define-map withdrawn-amounts
  { stream-id: uint, collaborator: principal }
  uint
)

(define-map artist-streams
  principal
  { stream-ids: (list 50 uint) }
)

(define-map subscriptions
  uint
  {
    subscriber: principal,
    artist: principal,
    amount: uint,
    duration-blocks: uint,
    start-block: uint,
    end-block: uint,
    auto-renew: bool,
    is-active: bool,
    total-paid: uint
  }
)

(define-map subscription-tiers
  { artist: principal, tier-name: (string-ascii 50) }
  {
    price: uint,
    duration-blocks: uint,
    benefits: (string-ascii 200)
  }
)

(define-map artist-subscriptions
  principal
  { subscription-ids: (list 100 uint) }
)

(define-map subscriber-subscriptions
  principal
  { subscription-ids: (list 20 uint) }
)

(define-map subscription-revenue
  { artist: principal, period: uint }
  { total-revenue: uint, subscriber-count: uint }
)

(define-map fan-points
  principal
  { 
    total-points: uint,
    current-points: uint,
    points-earned-today: uint,
    last-activity-block: uint,
    level: uint,
    lifetime-spent: uint
  }
)

(define-map artist-rewards
  { artist: principal, reward-id: uint }
  {
    reward-type: (string-ascii 50),
    points-cost: uint,
    max-redemptions: uint,
    current-redemptions: uint,
    is-active: bool,
    description: (string-ascii 200),
    expires-at-block: uint
  }
)

(define-map reward-redemptions
  { fan: principal, reward-id: uint, artist: principal }
  {
    redeemed-at-block: uint,
    redemption-id: uint
  }
)

(define-map fan-levels
  uint
  {
    level-name: (string-ascii 30),
    points-required: uint,
    daily-bonus-multiplier: uint
  }
)

(define-map fan-activity-log
  { fan: principal, activity-block: uint }
  {
    activity-type: (string-ascii 30),
    points-earned: uint,
    related-stream-id: uint
  }
)

(define-map daily-leaderboard
  { date: uint, rank: uint }
  {
    fan: principal,
    points-earned: uint
  }
)

(define-public (create-stream (title (string-ascii 100)) (collaborator-list (list 10 principal)) (percentages (list 10 uint)))
  (let
    (
      (stream-id (var-get next-stream-id))
      (total-percentage (fold + percentages u0))
    )
    (asserts! (<= total-percentage u10000) ERR_INVALID_PERCENTAGE)
    (asserts! (is-eq (len collaborator-list) (len percentages)) ERR_INVALID_PERCENTAGE)
    
    (map-set streams stream-id {
      artist: tx-sender,
      title: title,
      total-revenue: u0,
      is-active: true,
      created-at: stacks-block-height
    })
    
    (map-set stream-collaborators stream-id {
      collaborator-list: collaborator-list
    })
    
    ;; (map set-collaborator-percentage { stream-id: stream-id, collaborators: collaborator-list, percentages: percentages })
    
    (match (map-get? artist-streams tx-sender)
      existing-streams 
        (map-set artist-streams tx-sender {
          stream-ids: (unwrap! (as-max-len? (append (get stream-ids existing-streams) stream-id) u50) ERR_INVALID_AMOUNT)
        })
      (map-set artist-streams tx-sender { stream-ids: (list stream-id) })
    )
    
    (var-set next-stream-id (+ stream-id u1))
    (ok stream-id)
  )
)

(define-private (set-collaborator-percentage (data { stream-id: uint, collaborator: (list 10 principal), percentages: (list 10 uint) }))
  (let
    (
      (stream-id (get stream-id data))
      (collaborator (get collaborator data))
      (percentages (get percentages data))
    )
    (map set-single-collaborator-percentage 
      (map make-collaborator-data collaborator percentages)
    )
  )
)

(define-private (make-collaborator-data (collaborator principal) (percentage uint))
  { collaborator: collaborator, percentage: percentage }
)

(define-private (set-single-collaborator-percentage (data { collaborator: principal, percentage: uint }))
  (map-set collaborators 
    { stream-id: (var-get next-stream-id), collaborator: (get collaborator data) }
    { percentage: (get percentage data) }
  )
)

(define-public (deposit-revenue (stream-id uint) (amount uint))
  (let
    (
      (stream (unwrap! (map-get? streams stream-id) ERR_NOT_FOUND))
      (platform-fee (/ (* amount (var-get platform-fee-percentage)) u10000))
      (net-amount (- amount platform-fee))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (get is-active stream) ERR_STREAM_INACTIVE)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set streams stream-id 
      (merge stream { total-revenue: (+ (get total-revenue stream) net-amount) })
    )
    
    (map-set revenue-deposits 
      { stream-id: stream-id, depositor: tx-sender }
      { amount: amount, timestamp: stacks-block-height }
    )
    
    (unwrap-panic (award-points-for-activity tx-sender "revenue-deposit" (/ amount u1000000) stream-id))
    (unwrap-panic (track-revenue (get artist stream) net-amount))
    (ok net-amount)
  )
)

(define-public (withdraw-royalties (stream-id uint))
  (let
    (
      (stream (unwrap! (map-get? streams stream-id) ERR_NOT_FOUND))
      (collaborator-percentage (unwrap! (map-get? collaborators { stream-id: stream-id, collaborator: tx-sender }) ERR_UNAUTHORIZED))
      (total-revenue (get total-revenue stream))
      (entitled-amount (/ (* total-revenue (get percentage collaborator-percentage)) u10000))
      (already-withdrawn (default-to u0 (map-get? withdrawn-amounts { stream-id: stream-id, collaborator: tx-sender })))
      (withdrawable-amount (- entitled-amount already-withdrawn))
    )
    (asserts! (> withdrawable-amount u0) ERR_INSUFFICIENT_BALANCE)
    (asserts! (get is-active stream) ERR_STREAM_INACTIVE)
    
    (try! (as-contract (stx-transfer? withdrawable-amount tx-sender tx-sender)))
    
    (map-set withdrawn-amounts 
      { stream-id: stream-id, collaborator: tx-sender }
      entitled-amount
    )
    
    (ok withdrawable-amount)
  )
)

(define-public (toggle-stream-status (stream-id uint))
  (let
    (
      (stream (unwrap! (map-get? streams stream-id) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get artist stream)) ERR_UNAUTHORIZED)
    
    (map-set streams stream-id 
      (merge stream { is-active: (not (get is-active stream)) })
    )
    
    (ok (not (get is-active stream)))
  )
)

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee u1000) ERR_INVALID_PERCENTAGE)
    (var-set platform-fee-percentage new-fee)
    (ok new-fee)
  )
)

(define-read-only (get-stream (stream-id uint))
  (map-get? streams stream-id)
)

(define-read-only (get-collaborator-percentage (stream-id uint) (collaborator principal))
  (map-get? collaborators { stream-id: stream-id, collaborator: collaborator })
)

(define-read-only (get-withdrawable-amount (stream-id uint) (collaborator principal))
  (match (map-get? streams stream-id)
    stream
      (match (map-get? collaborators { stream-id: stream-id, collaborator: collaborator })
        collab-data
          (let
            (
              (total-revenue (get total-revenue stream))
              (entitled-amount (/ (* total-revenue (get percentage collab-data)) u10000))
              (already-withdrawn (default-to u0 (map-get? withdrawn-amounts { stream-id: stream-id, collaborator: collaborator })))
            )
            (ok (- entitled-amount already-withdrawn))
          )
        ERR_NOT_FOUND
      )
    ERR_NOT_FOUND
  )
)

(define-read-only (get-stream-collaborators (stream-id uint))
  (map-get? stream-collaborators stream-id)
)

(define-read-only (get-artist-streams (artist principal))
  (map-get? artist-streams artist)
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

(define-read-only (get-total-withdrawn (stream-id uint) (collaborator principal))
  (default-to u0 (map-get? withdrawn-amounts { stream-id: stream-id, collaborator: collaborator }))
)

(define-read-only (get-revenue-deposit (stream-id uint) (depositor principal))
  (map-get? revenue-deposits { stream-id: stream-id, depositor: depositor })
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

(define-public (create-subscription-tier (tier-name (string-ascii 50)) (price uint) (duration-blocks uint) (benefits (string-ascii 200)))
  (begin
    (asserts! (> price u0) ERR_INVALID_AMOUNT)
    (asserts! (> duration-blocks u0) ERR_INVALID_DURATION)
    (asserts! (is-none (map-get? subscription-tiers { artist: tx-sender, tier-name: tier-name })) ERR_ALREADY_EXISTS)
    
    (map-set subscription-tiers 
      { artist: tx-sender, tier-name: tier-name }
      { price: price, duration-blocks: duration-blocks, benefits: benefits }
    )
    
    (ok true)
  )
)

(define-public (subscribe-to-artist (artist principal) (tier-name (string-ascii 50)) (auto-renew bool))
  (let
    (
      (subscription-id (var-get next-subscription-id))
      (tier (unwrap! (map-get? subscription-tiers { artist: artist, tier-name: tier-name }) ERR_NOT_FOUND))
      (price (get price tier))
      (duration-blocks (get duration-blocks tier))
      (start-block stacks-block-height)
      (end-block (+ start-block duration-blocks))
      (platform-fee (/ (* price (var-get platform-fee-percentage)) u10000))
      (artist-amount (- price platform-fee))
    )
    (asserts! (> price u0) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
    
    (map-set subscriptions subscription-id {
      subscriber: tx-sender,
      artist: artist,
      amount: price,
      duration-blocks: duration-blocks,
      start-block: start-block,
      end-block: end-block,
      auto-renew: auto-renew,
      is-active: true,
      total-paid: price
    })
    
    (match (map-get? artist-subscriptions artist)
      existing-subs 
        (map-set artist-subscriptions artist {
          subscription-ids: (unwrap! (as-max-len? (append (get subscription-ids existing-subs) subscription-id) u100) ERR_INVALID_AMOUNT)
        })
      (map-set artist-subscriptions artist { subscription-ids: (list subscription-id) })
    )
    
    (match (map-get? subscriber-subscriptions tx-sender)
      existing-subs 
        (map-set subscriber-subscriptions tx-sender {
          subscription-ids: (unwrap! (as-max-len? (append (get subscription-ids existing-subs) subscription-id) u20) ERR_INVALID_AMOUNT)
        })
      (map-set subscriber-subscriptions tx-sender { subscription-ids: (list subscription-id) })
    )
    
    (try! (distribute-subscription-revenue artist artist-amount))
    (unwrap-panic (award-points-for-activity tx-sender "subscription" u50 u0))
    
    (var-set next-subscription-id (+ subscription-id u1))
    (ok subscription-id)
  )
)

(define-private (distribute-subscription-revenue (artist principal) (amount uint))
  (match (map-get? artist-streams artist)
    artist-stream-data
      (let
        (
          (stream-ids (get stream-ids artist-stream-data))
          (stream-count (len stream-ids))
        )
        (if (> stream-count u0)
          (fold distribute-single-stream stream-ids (ok true))
          (ok true)
        )
      )
    (ok true)
  )
)

(define-private (distribute-single-stream (stream-id uint) (prev-result (response bool uint)))
  (match (map-get? streams stream-id)
    stream
      (if (get is-active stream)
        (begin
          (map-set streams stream-id 
            (merge stream { total-revenue: (+ (get total-revenue stream) u1000) })
          )
          (ok true)
        )
        prev-result
      )
    prev-result
  )
)

(define-public (renew-subscription (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions subscription-id) ERR_SUBSCRIPTION_NOT_FOUND))
      (tier (unwrap! (map-get? subscription-tiers { artist: (get artist subscription), tier-name: "" }) ERR_NOT_FOUND))
      (price (get amount subscription))
      (duration-blocks (get duration-blocks subscription))
      (new-end-block (+ stacks-block-height duration-blocks))
      (platform-fee (/ (* price (var-get platform-fee-percentage)) u10000))
      (artist-amount (- price platform-fee))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription)) ERR_UNAUTHORIZED)
    (asserts! (get is-active subscription) ERR_SUBSCRIPTION_NOT_FOUND)
    (asserts! (>= stacks-block-height (get end-block subscription)) ERR_SUBSCRIPTION_EXPIRED)
    
    (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
    
    (map-set subscriptions subscription-id 
      (merge subscription { 
        end-block: new-end-block,
        total-paid: (+ (get total-paid subscription) price)
      })
    )
    
    (try! (distribute-subscription-revenue (get artist subscription) artist-amount))
    
    (ok new-end-block)
  )
)

(define-public (cancel-subscription (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions subscription-id) ERR_SUBSCRIPTION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription)) ERR_UNAUTHORIZED)
    (asserts! (get is-active subscription) ERR_SUBSCRIPTION_NOT_FOUND)
    
    (map-set subscriptions subscription-id 
      (merge subscription { is-active: false, auto-renew: false })
    )
    
    (ok true)
  )
)

(define-public (toggle-auto-renew (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions subscription-id) ERR_SUBSCRIPTION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription)) ERR_UNAUTHORIZED)
    (asserts! (get is-active subscription) ERR_SUBSCRIPTION_NOT_FOUND)
    
    (map-set subscriptions subscription-id 
      (merge subscription { auto-renew: (not (get auto-renew subscription)) })
    )
    
    (ok (not (get auto-renew subscription)))
  )
)

(define-read-only (get-subscription (subscription-id uint))
  (map-get? subscriptions subscription-id)
)

(define-read-only (get-subscription-tier (artist principal) (tier-name (string-ascii 50)))
  (map-get? subscription-tiers { artist: artist, tier-name: tier-name })
)

(define-read-only (is-subscription-active (subscription-id uint))
  (match (map-get? subscriptions subscription-id)
    subscription
      (and 
        (get is-active subscription)
        (< stacks-block-height (get end-block subscription))
      )
    false
  )
)

(define-read-only (get-artist-subscriptions (artist principal))
  (map-get? artist-subscriptions artist)
)

(define-read-only (get-subscriber-subscriptions (subscriber principal))
  (map-get? subscriber-subscriptions subscriber)
)

(define-read-only (get-subscription-revenue (artist principal) (period uint))
  (map-get? subscription-revenue { artist: artist, period: period })
)

(define-private (award-points-for-activity (fan principal) (activity-type (string-ascii 30)) (base-points uint) (stream-id uint))
  (let
    (
      (current-data (default-to { total-points: u0, current-points: u0, points-earned-today: u0, last-activity-block: u0, level: u1, lifetime-spent: u0 } 
                                (map-get? fan-points fan)))
      (is-same-day (is-eq (/ stacks-block-height u144) (/ (get last-activity-block current-data) u144)))
      (daily-points (if is-same-day (get points-earned-today current-data) u0))
      (level-data (default-to { level-name: "Bronze", points-required: u0, daily-bonus-multiplier: u100 } 
                              (map-get? fan-levels (get level current-data))))
      (bonus-multiplier (get daily-bonus-multiplier level-data))
      (final-points (/ (* base-points bonus-multiplier) u100))
      (new-total-points (+ (get total-points current-data) final-points))
      (new-current-points (+ (get current-points current-data) final-points))
      (new-daily-points (+ daily-points final-points))
      (new-level (calculate-level new-total-points))
    )
    (map-set fan-points fan {
      total-points: new-total-points,
      current-points: new-current-points,
      points-earned-today: new-daily-points,
      last-activity-block: stacks-block-height,
      level: new-level,
      lifetime-spent: (get lifetime-spent current-data)
    })
    
    (map-set fan-activity-log 
      { fan: fan, activity-block: stacks-block-height }
      { activity-type: activity-type, points-earned: final-points, related-stream-id: stream-id }
    )
    
    (ok final-points)
  )
)

(define-private (calculate-level (total-points uint))
  (if (< total-points u500) u1
    (if (< total-points u1500) u2
      (if (< total-points u5000) u3
        (if (< total-points u15000) u4
          u5
        )
      )
    )
  )
)

(define-public (create-artist-reward (reward-type (string-ascii 50)) (points-cost uint) (max-redemptions uint) (description (string-ascii 200)) (expires-in-blocks uint))
  (let
    (
      (reward-id (var-get next-reward-id))
      (expires-at (+ stacks-block-height expires-in-blocks))
    )
    (asserts! (> points-cost u0) ERR_INVALID_POINTS)
    (asserts! (> max-redemptions u0) ERR_INVALID_AMOUNT)
    
    (map-set artist-rewards 
      { artist: tx-sender, reward-id: reward-id }
      {
        reward-type: reward-type,
        points-cost: points-cost,
        max-redemptions: max-redemptions,
        current-redemptions: u0,
        is-active: true,
        description: description,
        expires-at-block: expires-at
      }
    )
    
    (var-set next-reward-id (+ reward-id u1))
    (ok reward-id)
  )
)

(define-public (redeem-reward (artist principal) (reward-id uint))
  (let
    (
      (fan-data (unwrap! (map-get? fan-points tx-sender) ERR_NOT_FOUND))
      (reward (unwrap! (map-get? artist-rewards { artist: artist, reward-id: reward-id }) ERR_REWARD_NOT_FOUND))
      (points-cost (get points-cost reward))
      (current-points (get current-points fan-data))
    )
    (asserts! (get is-active reward) ERR_REWARD_UNAVAILABLE)
    (asserts! (< stacks-block-height (get expires-at-block reward)) ERR_REWARD_UNAVAILABLE)
    (asserts! (< (get current-redemptions reward) (get max-redemptions reward)) ERR_REWARD_UNAVAILABLE)
    (asserts! (>= current-points points-cost) ERR_INSUFFICIENT_POINTS)
    
    (map-set fan-points tx-sender 
      (merge fan-data { 
        current-points: (- current-points points-cost),
        lifetime-spent: (+ (get lifetime-spent fan-data) points-cost)
      })
    )
    
    (map-set artist-rewards 
      { artist: artist, reward-id: reward-id }
      (merge reward { current-redemptions: (+ (get current-redemptions reward) u1) })
    )
    
    (map-set reward-redemptions 
      { fan: tx-sender, reward-id: reward-id, artist: artist }
      { redeemed-at-block: stacks-block-height, redemption-id: reward-id }
    )
    
    (ok true)
  )
)

(define-public (initialize-fan-levels)
  (begin
    (map-set fan-levels u1 { level-name: "Bronze", points-required: u0, daily-bonus-multiplier: u100 })
    (map-set fan-levels u2 { level-name: "Silver", points-required: u500, daily-bonus-multiplier: u110 })
    (map-set fan-levels u3 { level-name: "Gold", points-required: u1500, daily-bonus-multiplier: u125 })
    (map-set fan-levels u4 { level-name: "Platinum", points-required: u5000, daily-bonus-multiplier: u150 })
    (map-set fan-levels u5 { level-name: "Diamond", points-required: u15000, daily-bonus-multiplier: u200 })
    (ok true)
  )
)

(define-public (update-daily-leaderboard (date uint))
  (let
    (
      (fan-data (unwrap! (map-get? fan-points tx-sender) ERR_NOT_FOUND))
      (daily-points (get points-earned-today fan-data))
    )
    (asserts! (> daily-points u0) ERR_INVALID_POINTS)
    
    (map-set daily-leaderboard 
      { date: date, rank: u1 }
      { fan: tx-sender, points-earned: daily-points }
    )
    
    (ok daily-points)
  )
)

(define-public (claim-daily-bonus)
  (let
    (
      (fan-data (unwrap! (map-get? fan-points tx-sender) ERR_NOT_FOUND))
      (last-claim-day (/ (get last-activity-block fan-data) u144))
      (current-day (/ stacks-block-height u144))
      (level-data (default-to { level-name: "Bronze", points-required: u0, daily-bonus-multiplier: u100 } 
                              (map-get? fan-levels (get level fan-data))))
      (daily-bonus (/ (* u25 (get daily-bonus-multiplier level-data)) u100))
    )
    (asserts! (> current-day last-claim-day) ERR_INVALID_AMOUNT)
    
    (unwrap-panic (award-points-for-activity tx-sender "daily-bonus" daily-bonus u0))
    
    (ok daily-bonus)
  )
)

(define-read-only (get-fan-points (fan principal))
  (map-get? fan-points fan)
)

(define-read-only (get-artist-reward (artist principal) (reward-id uint))
  (map-get? artist-rewards { artist: artist, reward-id: reward-id })
)

(define-read-only (get-fan-level (level uint))
  (map-get? fan-levels level)
)

(define-read-only (get-fan-activity (fan principal) (activity-block uint))
  (map-get? fan-activity-log { fan: fan, activity-block: activity-block })
)

(define-read-only (get-daily-leaderboard-entry (date uint) (rank uint))
  (map-get? daily-leaderboard { date: date, rank: rank })
)

(define-read-only (get-reward-redemption (fan principal) (reward-id uint) (artist principal))
  (map-get? reward-redemptions { fan: fan, reward-id: reward-id, artist: artist })
)

(define-read-only (calculate-fan-rank (fan principal))
  (match (map-get? fan-points fan)
    fan-data
      (let
        (
          (total-points (get total-points fan-data))
          (level (get level fan-data))
        )
        (ok { total-points: total-points, level: level, rank: (+ level total-points) })
      )
    ERR_NOT_FOUND
  )
)

;; Analytics tracking helpers
(define-private (track-revenue (artist principal) (amount uint))
  (let ((month-id (/ stacks-block-height u4320))) ;; Approximate monthly blocks
    (contract-call? .musiflow-analytics record-revenue artist month-id amount)
  )
)

(define-private (track-activity (artist principal) (plays uint) (likes uint) (purchases uint))
  (let ((month-id (/ stacks-block-height u4320)))
    (contract-call? .musiflow-analytics record-fan-activity artist month-id plays likes purchases)
  )
)


