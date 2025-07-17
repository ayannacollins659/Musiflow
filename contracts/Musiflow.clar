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

(define-data-var next-stream-id uint u1)
(define-data-var platform-fee-percentage uint u250)
(define-data-var next-subscription-id uint u1)

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