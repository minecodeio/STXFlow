;; Payment Processing Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-merchant-exists (err u101))
(define-constant err-merchant-not-found (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-payment-failed (err u104))
(define-constant err-insufficient-balance (err u105))
(define-constant err-payment-not-found (err u106))
(define-constant err-payment-already-processed (err u107))

;; Define data maps
(define-map merchants
  { id: (string-ascii 64) }
  {
    name: (string-ascii 64),
    wallet: principal,
    fee-percentage: uint,
    active: bool,
    total-received: uint,
    created-at: uint
  }
)

(define-map payments
  { id: uint }
  {
    merchant-id: (string-ascii 64),
    customer: principal,
    amount: uint,
    fee: uint,
    net-amount: uint,
    status: (string-ascii 16),
    memo: (optional (string-ascii 256)),
    created-at: uint,
    processed-at: (optional uint)
  }
)

;; Define variables
(define-data-var payment-counter uint u0)
(define-data-var platform-fee-percentage uint u250) ;; 2.5% (represented as basis points)
(define-data-var platform-wallet principal contract-owner)
(define-data-var total-processed-amount uint u0)
(define-data-var total-fees-collected uint u0)

;; Register a new merchant
(define-public (register-merchant (merchant-id (string-ascii 64)) (name (string-ascii 64)) (wallet principal))
  (begin
    (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender wallet)) (err err-not-authorized))
    (asserts! (is-none (map-get? merchants { id: merchant-id })) (err err-merchant-exists))
    
    (map-set merchants
      { id: merchant-id }
      {
        name: name,
        wallet: wallet,
        fee-percentage: (var-get platform-fee-percentage),
        active: true,
        total-received: u0,
        created-at: stacks-block-height
      }
    )
    
    (ok true)
  )
)

;; Update merchant details
(define-public (update-merchant (merchant-id (string-ascii 64)) (name (string-ascii 64)) (wallet principal))
  (let
    (
      (merchant (unwrap! (map-get? merchants { id: merchant-id }) (err err-merchant-not-found)))
    )
    (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (get wallet merchant))) (err err-not-authorized))
    
    (map-set merchants
      { id: merchant-id }
      (merge merchant {
        name: name,
        wallet: wallet
      })
    )
    
    (ok true)
  )
)

;; Deactivate a merchant
(define-public (deactivate-merchant (merchant-id (string-ascii 64)))
  (let
    (
      (merchant (unwrap! (map-get? merchants { id: merchant-id }) (err err-merchant-not-found)))
    )
    (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (get wallet merchant))) (err err-not-authorized))
    
    (map-set merchants
      { id: merchant-id }
      (merge merchant { active: false })
    )
    
    (ok true)
  )
)

;; Reactivate a merchant
(define-public (reactivate-merchant (merchant-id (string-ascii 64)))
  (let
    (
      (merchant (unwrap! (map-get? merchants { id: merchant-id }) (err err-merchant-not-found)))
    )
    (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (get wallet merchant))) (err err-not-authorized))
    
    (map-set merchants
      { id: merchant-id }
      (merge merchant { active: true })
    )
    
    (ok true)
  )
)

;; Process a payment
(define-public (process-payment (merchant-id (string-ascii 64)) (amount uint) (memo (optional (string-ascii 256))))
  (let
    (
      (merchant (unwrap! (map-get? merchants { id: merchant-id }) (err err-merchant-not-found)))
      (payment-id (+ (var-get payment-counter) u1))
      (fee-percentage (get fee-percentage merchant))
      (fee (/ (* amount fee-percentage) u10000))
      (net-amount (- amount fee))
    )
    (asserts! (get active merchant) (err err-merchant-not-found))
    (asserts! (> amount u0) (err err-invalid-amount))
    
    ;; Transfer STX from customer to contract
    (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender)) (err err-payment-failed))
    
    ;; Record the payment
    (map-set payments
      { id: payment-id }
      {
        merchant-id: merchant-id,
        customer: tx-sender,
        amount: amount,
        fee: fee,
        net-amount: net-amount,
        status: "pending",
        memo: memo,
        created-at: stacks-block-height,
        processed-at: none
      }
    )
    
    (var-set payment-counter payment-id)
    
    ;; Complete the payment immediately
    (complete-payment payment-id)
  )
)

;; Complete a payment (internal function)
(define-private (complete-payment (payment-id uint))
  (let
    (
      (payment (unwrap! (map-get? payments { id: payment-id }) (err err-payment-not-found)))
      (merchant (unwrap! (map-get? merchants { id: (get merchant-id payment) }) (err err-merchant-not-found)))
    )
    (asserts! (is-eq (get status payment) "pending") (err err-payment-already-processed))
    
    ;; Transfer net amount to merchant
    (unwrap! (as-contract (stx-transfer? (get net-amount payment) tx-sender (get wallet merchant))) (err err-payment-failed))
    
    ;; Transfer fee to platform wallet
    (unwrap! (as-contract (stx-transfer? (get fee payment) tx-sender (var-get platform-wallet))) (err err-payment-failed))
    
    ;; Update payment status
    (map-set payments
      { id: payment-id }
      (merge payment {
        status: "completed",
        processed-at: (some stacks-block-height)
      })
    )
    
    ;; Update merchant stats
    (map-set merchants
      { id: (get merchant-id payment) }
      (merge merchant {
        total-received: (+ (get total-received merchant) (get net-amount payment))
      })
    )
    
    ;; Update platform stats
    (var-set total-processed-amount (+ (var-get total-processed-amount) (get amount payment)))
    (var-set total-fees-collected (+ (var-get total-fees-collected) (get fee payment)))
    
    (ok payment-id)
  )
)

;; Update platform fee percentage (only contract owner)
(define-public (update-platform-fee (new-fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-not-authorized))
    (asserts! (<= new-fee-percentage u1000) (err err-invalid-amount)) ;; Max 10%
    
    (var-set platform-fee-percentage new-fee-percentage)
    
    (ok true)
  )
)

;; Update platform wallet (only contract owner)
(define-public (update-platform-wallet (new-wallet principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-not-authorized))
    
    (var-set platform-wallet new-wallet)
    
    (ok true)
  )
)

;; Get merchant details
(define-read-only (get-merchant (merchant-id (string-ascii 64)))
  (map-get? merchants { id: merchant-id })
)

;; Get payment details
(define-read-only (get-payment (payment-id uint))
  (map-get? payments { id: payment-id })
)

;; Get platform fee percentage
(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

;; Get platform wallet
(define-read-only (get-platform-wallet)
  (var-get platform-wallet)
)

;; Get total processed amount
(define-read-only (get-total-processed-amount)
  (var-get total-processed-amount)
)

;; Get total fees collected
(define-read-only (get-total-fees-collected)
  (var-get total-fees-collected)
)

;; Get contract balance
(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

;; Validate a payment (for external verification)
(define-read-only (validate-payment (payment-id uint))
  (let
    (
      (payment (map-get? payments { id: payment-id }))
    )
    (and
      (is-some payment)
      (is-eq (get status (unwrap-panic payment)) "completed")
      (is-some (get processed-at (unwrap-panic payment)))
    )
  )
)