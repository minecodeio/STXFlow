;; Escrow Smart Contract
;; Holds funds securely until delivery conditions are met

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ESCROW_NOT_FOUND (err u101))
(define-constant ERR_ESCROW_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_ESCROW_EXPIRED (err u104))
(define-constant ERR_ESCROW_NOT_EXPIRED (err u105))
(define-constant ERR_ESCROW_ALREADY_RELEASED (err u106))
(define-constant ERR_ESCROW_ALREADY_REFUNDED (err u107))
(define-constant ERR_INVALID_STATUS (err u108))

;; Escrow status definitions
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_DELIVERED u2)
(define-constant STATUS_RELEASED u3)
(define-constant STATUS_REFUNDED u4)
(define-constant STATUS_DISPUTED u5)

;; Data Variables
(define-data-var escrow-counter uint u0)
(define-data-var platform-fee-rate uint u250) ;; 2.5% (250 basis points)

;; Data Maps
(define-map escrows
  uint
  {
    buyer: principal,
    seller: principal,
    amount: uint,
    timeout-block: uint,
    status: uint,
    description: (string-ascii 256),
    created-block: uint
  }
)

(define-map escrow-fees
  uint
  uint ;; fee amount collected
)

;; Read-only functions

;; Get escrow details by ID
(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows escrow-id)
)

;; Get current escrow counter
(define-read-only (get-escrow-counter)
  (var-get escrow-counter)
)

;; Get platform fee rate
(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

;; Calculate platform fee for given amount
(define-read-only (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-rate)) u10000)
)

;; Check if escrow has expired
(define-read-only (is-escrow-expired (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow-data (> stacks-block-height (get timeout-block escrow-data))
    false
  )
)

;; Get escrow status string
(define-read-only (get-status-string (status uint))
  (if (is-eq status STATUS_ACTIVE)
    "ACTIVE"
    (if (is-eq status STATUS_DELIVERED)
      "DELIVERED"
      (if (is-eq status STATUS_RELEASED)
        "RELEASED"
        (if (is-eq status STATUS_REFUNDED)
          "REFUNDED"
          (if (is-eq status STATUS_DISPUTED)
            "DISPUTED"
            "UNKNOWN"
          )
        )
      )
    )
  )
)

;; Public functions

;; Create new escrow
(define-public (create-escrow 
    (seller principal) 
    (amount uint) 
    (timeout-blocks uint) 
    (description (string-ascii 256)))
  (let (
    (escrow-id (+ (var-get escrow-counter) u1))
    (timeout-block (+ stacks-block-height timeout-blocks))
    (platform-fee (calculate-platform-fee amount))
    (total-amount (+ amount platform-fee))
  )
    ;; Check if buyer has sufficient funds
    (asserts! (>= (stx-get-balance tx-sender) total-amount) ERR_INSUFFICIENT_FUNDS)
    
    ;; Transfer funds to contract
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    ;; Create escrow record
    (map-set escrows escrow-id {
      buyer: tx-sender,
      seller: seller,
      amount: amount,
      timeout-block: timeout-block,
      status: STATUS_ACTIVE,
      description: description,
      created-block: stacks-block-height
    })
    
    ;; Store fee amount
    (map-set escrow-fees escrow-id platform-fee)
    
    ;; Update counter
    (var-set escrow-counter escrow-id)
    
    ;; Print event
    (print {
      event: "escrow-created",
      escrow-id: escrow-id,
      buyer: tx-sender,
      seller: seller,
      amount: amount,
      timeout-block: timeout-block
    })
    
    (ok escrow-id)
  )
)

;; Buyer confirms delivery
(define-public (confirm-delivery (escrow-id uint))
  (let (
    (escrow-data (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
  )
    ;; Check authorization
    (asserts! (is-eq tx-sender (get buyer escrow-data)) ERR_UNAUTHORIZED)
    
    ;; Check escrow is active
    (asserts! (is-eq (get status escrow-data) STATUS_ACTIVE) ERR_INVALID_STATUS)
    
    ;; Update status to delivered
    (map-set escrows escrow-id
      (merge escrow-data { status: STATUS_DELIVERED })
    )
    
    ;; Print event
    (print {
      event: "delivery-confirmed",
      escrow-id: escrow-id,
      buyer: tx-sender
    })
    
    (ok true)
  )
)

;; Release funds to seller (can be called by buyer after confirming delivery or automatically after timeout)
(define-public (release-funds (escrow-id uint))
  (let (
    (escrow-data (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
    (platform-fee (default-to u0 (map-get? escrow-fees escrow-id)))
  )
    ;; Check authorization (buyer or seller can call, or anyone after timeout)
    (asserts! (or 
      (is-eq tx-sender (get buyer escrow-data))
      (is-eq tx-sender (get seller escrow-data))
      (> stacks-block-height (get timeout-block escrow-data))
    ) ERR_UNAUTHORIZED)
    
    ;; Check escrow status (must be delivered or expired active)
    (asserts! (or 
      (is-eq (get status escrow-data) STATUS_DELIVERED)
      (and 
        (is-eq (get status escrow-data) STATUS_ACTIVE)
        (> stacks-block-height (get timeout-block escrow-data))
      )
    ) ERR_INVALID_STATUS)
    
    ;; Transfer funds to seller
    (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get seller escrow-data))))
    
    ;; Transfer platform fee to contract owner
    (if (> platform-fee u0)
      (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
      true
    )
    
    ;; Update status
    (map-set escrows escrow-id
      (merge escrow-data { status: STATUS_RELEASED })
    )
    
    ;; Print event
    (print {
      event: "funds-released",
      escrow-id: escrow-id,
      seller: (get seller escrow-data),
      amount: (get amount escrow-data)
    })
    
    (ok true)
  )
)

;; Refund to buyer (only before timeout and if not delivered)
(define-public (refund-escrow (escrow-id uint))
  (let (
    (escrow-data (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
    (platform-fee (default-to u0 (map-get? escrow-fees escrow-id)))
  )
    ;; Check authorization (only seller can initiate refund)
    (asserts! (is-eq tx-sender (get seller escrow-data)) ERR_UNAUTHORIZED)
    
    ;; Check escrow is active and not expired
    (asserts! (is-eq (get status escrow-data) STATUS_ACTIVE) ERR_INVALID_STATUS)
    (asserts! (<= stacks-block-height (get timeout-block escrow-data)) ERR_ESCROW_EXPIRED)
    
    ;; Transfer full amount back to buyer (including platform fee)
    (try! (as-contract (stx-transfer? (+ (get amount escrow-data) platform-fee) tx-sender (get buyer escrow-data))))
    
    ;; Update status
    (map-set escrows escrow-id
      (merge escrow-data { status: STATUS_REFUNDED })
    )
    
    ;; Print event
    (print {
      event: "escrow-refunded",
      escrow-id: escrow-id,
      buyer: (get buyer escrow-data),
      amount: (get amount escrow-data)
    })
    
    (ok true)
  )
)

;; Dispute escrow (can be called by buyer or seller)
(define-public (dispute-escrow (escrow-id uint))
  (let (
    (escrow-data (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
  )
    ;; Check authorization
    (asserts! (or 
      (is-eq tx-sender (get buyer escrow-data))
      (is-eq tx-sender (get seller escrow-data))
    ) ERR_UNAUTHORIZED)
    
    ;; Check escrow is active
    (asserts! (is-eq (get status escrow-data) STATUS_ACTIVE) ERR_INVALID_STATUS)
    
    ;; Update status to disputed
    (map-set escrows escrow-id
      (merge escrow-data { status: STATUS_DISPUTED })
    )
    
    ;; Print event
    (print {
      event: "escrow-disputed",
      escrow-id: escrow-id,
      disputer: tx-sender
    })
    
    (ok true)
  )
)

;; Resolve dispute (only contract owner can resolve)
(define-public (resolve-dispute (escrow-id uint) (winner principal))
  (let (
    (escrow-data (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
    (platform-fee (default-to u0 (map-get? escrow-fees escrow-id)))
  )
    ;; Check authorization (only contract owner)
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    ;; Check escrow is disputed
    (asserts! (is-eq (get status escrow-data) STATUS_DISPUTED) ERR_INVALID_STATUS)
    
    ;; Check winner is either buyer or seller
    (asserts! (or 
      (is-eq winner (get buyer escrow-data))
      (is-eq winner (get seller escrow-data))
    ) ERR_UNAUTHORIZED)
    
    ;; Transfer funds based on resolution
    (if (is-eq winner (get seller escrow-data))
      ;; Seller wins - release funds
      (begin
        (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get seller escrow-data))))
        (if (> platform-fee u0)
          (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
          true
        )
        (map-set escrows escrow-id (merge escrow-data { status: STATUS_RELEASED }))
      )
      ;; Buyer wins - refund
      (begin
        (try! (as-contract (stx-transfer? (+ (get amount escrow-data) platform-fee) tx-sender (get buyer escrow-data))))
        (map-set escrows escrow-id (merge escrow-data { status: STATUS_REFUNDED }))
      )
    )
    
    ;; Print event
    (print {
      event: "dispute-resolved",
      escrow-id: escrow-id,
      winner: winner
    })
    
    (ok true)
  )
)

;; Admin function to update platform fee rate
(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-rate u1000) (err u109)) ;; Max 10%
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)