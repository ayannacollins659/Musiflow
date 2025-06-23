(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_ALREADY_EXISTS (err u104))
(define-constant ERR_INVALID_PERCENTAGE (err u105))
(define-constant ERR_STREAM_INACTIVE (err u106))

(define-data-var next-stream-id uint u1)
(define-data-var platform-fee-percentage uint u250)

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