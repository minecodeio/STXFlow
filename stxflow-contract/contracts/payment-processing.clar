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
