
;; title: Auto-refund
;; version:
;; summary:
;; description:
;; title: automated-refunds
;; version: 1.0.0
;; summary: A smart contract feature for handling automated refunds securely
;; description: This contract allows merchants to initiate and execute refunds directly
;;              through the blockchain, providing transparency and security for both parties.

;; Error codes
(define-constant ERR-NOT-INITIALIZED (err u100))
(define-constant ERR-ALREADY-INITIALIZED (err u101))
(define-constant ERR-UNAUTHORIZED (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-WRONG-STATE (err u104))
(define-constant ERR-TRANSFER-FAILED (err u105))
(define-constant ERR-EXPIRED-WINDOW (err u106))

;; Refund states
(define-constant STATE-NONE u0)
(define-constant STATE-INITIALIZED u1)
(define-constant STATE-PAYMENT-RECEIVED u2)
(define-constant STATE-REFUND-REQUESTED u3)
(define-constant STATE-REFUND-APPROVED u4)
(define-constant STATE-REFUND-COMPLETED u5)
(define-constant STATE-REFUND-DENIED u6)

;; Define the data structures
(define-data-var contract-state uint STATE-NONE)
(define-data-var merchant principal tx-sender)
(define-data-var customer principal tx-sender)
(define-data-var payment-amount uint u0)
(define-data-var payment-timestamp uint u0)
(define-data-var refund-window-blocks uint u144) ;; Default ~24 hours (144 blocks)
(define-data-var refund-reason (string-ascii 256) "")
(define-data-var refund-transaction-id (optional uint) none)

;; Read-only functions to get contract data
(define-read-only (get-contract-state)
  (var-get contract-state))

(define-read-only (get-merchant)
  (var-get merchant))

(define-read-only (get-customer)
  (var-get customer))

(define-read-only (get-payment-amount)
  (var-get payment-amount))

(define-read-only (get-payment-timestamp)
  (var-get payment-timestamp))

(define-read-only (get-refund-window)
  (var-get refund-window-blocks))

(define-read-only (get-refund-deadline)
  (+ (var-get payment-timestamp) (var-get refund-window-blocks)))

(define-read-only (is-refund-window-expired)
  (> block-height (get-refund-deadline)))

(define-read-only (get-refund-reason)
  (var-get refund-reason))

(define-read-only (get-refund-transaction)
  (var-get refund-transaction-id))

(define-read-only (get-refund-status)
  (let ((state (var-get contract-state)))
    (ok {
      state: state,
      is-initialized: (> state STATE-NONE),
      payment-received: (>= state STATE-PAYMENT-RECEIVED),
      refund-requested: (>= state STATE-REFUND-REQUESTED),
      refund-approved: (>= state STATE-REFUND-APPROVED),
      refund-completed: (is-eq state STATE-REFUND-COMPLETED),
      refund-denied: (is-eq state STATE-REFUND-DENIED),
      window-expired: (is-refund-window-expired)
    })
  ))

;; Function to initialize the contract
(define-public (initialize (merchant-principal principal) (customer-principal principal) (amount uint) (refund-blocks uint))
  (begin
    (asserts! (is-eq (var-get contract-state) STATE-NONE) (err ERR-ALREADY-INITIALIZED))
    (asserts! (> amount u0) (err ERR-INVALID-AMOUNT))
    (asserts! (not (is-eq merchant-principal customer-principal)) (err ERR-INVALID-AMOUNT))
    
    (var-set merchant merchant-principal)
    (var-set customer customer-principal)
    (var-set payment-amount amount)
    
    ;; Set a custom refund window if provided, otherwise use default
    (if (> refund-blocks u0)
        (var-set refund-window-blocks refund-blocks)
        true)
    
    (var-set contract-state STATE-INITIALIZED)
    (ok true)
  ))

;; Function for the customer to make a payment
(define-public (make-payment)
  (let ((amount (var-get payment-amount)))
    (begin
      (asserts! (is-eq (var-get contract-state) STATE-INITIALIZED) (err ERR-WRONG-STATE))
      (asserts! (is-eq tx-sender (var-get customer)) (err ERR-UNAUTHORIZED))
      
      (match (stx-transfer? amount tx-sender (as-contract tx-sender))
        success (begin
          (var-set payment-timestamp block-height)
          (var-set contract-state STATE-PAYMENT-RECEIVED)
          (ok true)
        )
        error (err ERR-TRANSFER-FAILED)
      )
    ))
)

;; Function for the merchant to request a refund
(define-public (request-refund (reason (string-ascii 256)))
  (begin
    (asserts! (>= (var-get contract-state) STATE-PAYMENT-RECEIVED) (err ERR-WRONG-STATE))
    (asserts! (< (var-get contract-state) STATE-REFUND-REQUESTED) (err ERR-WRONG-STATE))
    (asserts! (is-eq tx-sender (var-get merchant)) (err ERR-UNAUTHORIZED))
    (asserts! (not (is-refund-window-expired)) (err ERR-EXPIRED-WINDOW))
    
    (var-set refund-reason reason)
    (var-set contract-state STATE-REFUND-REQUESTED)
    (ok true)
  ))

;; Function for the customer to approve the refund
(define-public (approve-refund)
  (begin
    (asserts! (is-eq (var-get contract-state) STATE-REFUND-REQUESTED) (err ERR-WRONG-STATE))
    (asserts! (is-eq tx-sender (var-get customer)) (err ERR-UNAUTHORIZED))
    
    (var-set contract-state STATE-REFUND-APPROVED)
    (execute-refund)
  ))

;; Function for the customer to deny the refund
(define-public (deny-refund)
  (begin
    (asserts! (is-eq (var-get contract-state) STATE-REFUND-REQUESTED) (err ERR-WRONG-STATE))
    (asserts! (is-eq tx-sender (var-get customer)) (err ERR-UNAUTHORIZED))
    
    (var-set contract-state STATE-REFUND-DENIED)
    (ok true)
  ))

;; Private function to execute the refund
(define-private (execute-refund)
  (let ((amount (var-get payment-amount))
        (customer-addr (var-get customer))
        (tx-id u1)
        )
    (begin
      (asserts! (is-eq (var-get contract-state) STATE-REFUND-APPROVED) (err ERR-WRONG-STATE))
      
      (match (as-contract (stx-transfer? amount tx-sender customer-addr))
        success (begin
          (var-set refund-transaction-id (some tx-id))
          (var-set contract-state STATE-REFUND-COMPLETED)
          (ok true)
        )
        error (err ERR-TRANSFER-FAILED)
      )
    ))
)

;; Function for auto-refund if within refund window and merchant approved
(define-public (auto-refund)
  (begin
    (asserts! (is-eq (var-get contract-state) STATE-PAYMENT-RECEIVED) (err ERR-WRONG-STATE))
    (asserts! (is-eq tx-sender (var-get merchant)) (err ERR-UNAUTHORIZED))
    (asserts! (not (is-refund-window-expired)) (err ERR-EXPIRED-WINDOW))
    
    (var-set contract-state STATE-REFUND-APPROVED)
    (execute-refund)
  ))


;; Function to reset the contract state after completion
(define-public (reset-contract)
  (begin
    (asserts! (or (is-eq (var-get contract-state) STATE-REFUND-COMPLETED)
                 (is-eq (var-get contract-state) STATE-REFUND-DENIED)) (err ERR-WRONG-STATE))
    (asserts! (or (is-eq tx-sender (var-get merchant))
                 (is-eq tx-sender (var-get customer))) (err ERR-UNAUTHORIZED))
    
    (var-set contract-state STATE-NONE)
    (var-set payment-amount u0)
    (var-set payment-timestamp u0)
    (var-set refund-reason "")
    (var-set refund-transaction-id none)
    (ok true)
  ))