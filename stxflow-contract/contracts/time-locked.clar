;; Time-Locked Deals Smart Contract
;; Manages limited-time offers with automatic expiration

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-deal-not-found (err u101))
(define-constant err-deal-expired (err u102))
(define-constant err-deal-not-active (err u103))
(define-constant err-insufficient-payment (err u104))
(define-constant err-deal-sold-out (err u105))
(define-constant err-invalid-deal-params (err u106))

;; Data Variables
(define-data-var next-deal-id uint u1)

;; Data Maps
(define-map deals
  { deal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    original-price: uint,
    deal-price: uint,
    start-block: uint,
    end-block: uint,
    max-quantity: uint,
    sold-quantity: uint,
    is-active: bool,
    creator: principal
  }
)

(define-map user-purchases
  { user: principal, deal-id: uint }
  { quantity: uint, purchase-block: uint }
)

;; Read-only functions

(define-read-only (get-deal (deal-id uint))
  (map-get? deals { deal-id: deal-id })
)

(define-read-only (get-user-purchase (user principal) (deal-id uint))
  (map-get? user-purchases { user: user, deal-id: deal-id })
)

(define-read-only (is-deal-active (deal-id uint))
  (match (map-get? deals { deal-id: deal-id })
    deal-data 
    (and 
      (get is-active deal-data)
      (>= stacks-block-height (get start-block deal-data))
      (<= stacks-block-height (get end-block deal-data))
      (< (get sold-quantity deal-data) (get max-quantity deal-data))
    )
    false
  )
)

(define-read-only (get-remaining-quantity (deal-id uint))
  (match (map-get? deals { deal-id: deal-id })
    deal-data (- (get max-quantity deal-data) (get sold-quantity deal-data))
    u0
  )
)

(define-read-only (get-time-remaining (deal-id uint))
  (match (map-get? deals { deal-id: deal-id })
    deal-data 
    (if (<= stacks-block-height (get end-block deal-data))
      (- (get end-block deal-data) stacks-block-height)
      u0
    )
    u0
  )
)

(define-read-only (get-current-deal-id)
  (var-get next-deal-id)
)

;; Public functions

(define-public (create-deal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (original-price uint)
  (deal-price uint)
  (duration-blocks uint)
  (max-quantity uint)
)
  (let 
    (
      (deal-id (var-get next-deal-id))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height duration-blocks))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> original-price deal-price) err-invalid-deal-params)
    (asserts! (> duration-blocks u0) err-invalid-deal-params)
    (asserts! (> max-quantity u0) err-invalid-deal-params)
    
    (map-set deals
      { deal-id: deal-id }
      {
        title: title,
        description: description,
        original-price: original-price,
        deal-price: deal-price,
        start-block: start-block,
        end-block: end-block,
        max-quantity: max-quantity,
        sold-quantity: u0,
        is-active: true,
        creator: tx-sender
      }
    )
    
    (var-set next-deal-id (+ deal-id u1))
    (ok deal-id)
  )
)

(define-public (purchase-deal (deal-id uint) (quantity uint))
  (let 
    (
      (deal-data (unwrap! (map-get? deals { deal-id: deal-id }) err-deal-not-found))
      (total-cost (* (get deal-price deal-data) quantity))
      (current-sold (get sold-quantity deal-data))
      (new-sold (+ current-sold quantity))
    )
    ;; Check if deal exists and is active
    (asserts! (is-deal-active deal-id) err-deal-expired)
    
    ;; Check if enough quantity available
    (asserts! (<= new-sold (get max-quantity deal-data)) err-deal-sold-out)
    
    ;; Check if payment is sufficient (in a real implementation, you'd handle STX transfer)
    ;; For this example, we assume payment verification happens elsewhere
    
    ;; Update sold quantity
    (map-set deals
      { deal-id: deal-id }
      (merge deal-data { sold-quantity: new-sold })
    )
    
    ;; Record user purchase
    (match (map-get? user-purchases { user: tx-sender, deal-id: deal-id })
      existing-purchase
      (map-set user-purchases
        { user: tx-sender, deal-id: deal-id }
        {
          quantity: (+ (get quantity existing-purchase) quantity),
          purchase-block: stacks-block-height
        }
      )
      (map-set user-purchases
        { user: tx-sender, deal-id: deal-id }
        {
          quantity: quantity,
          purchase-block: stacks-block-height
        }
      )
    )
    
    (ok { deal-id: deal-id, quantity: quantity, total-cost: total-cost })
  )
)

(define-public (deactivate-deal (deal-id uint))
  (let 
    (
      (deal-data (unwrap! (map-get? deals { deal-id: deal-id }) err-deal-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set deals
      { deal-id: deal-id }
      (merge deal-data { is-active: false })
    )
    
    (ok true)
  )
)

(define-public (extend-deal (deal-id uint) (additional-blocks uint))
  (let 
    (
      (deal-data (unwrap! (map-get? deals { deal-id: deal-id }) err-deal-not-found))
      (new-end-block (+ (get end-block deal-data) additional-blocks))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get is-active deal-data) err-deal-not-active)
    
    (map-set deals
      { deal-id: deal-id }
      (merge deal-data { end-block: new-end-block })
    )
    
    (ok new-end-block)
  )
)

;; Emergency functions (owner only)

(define-public (emergency-stop-deal (deal-id uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (deactivate-deal deal-id)
  )
)

;; Initialize contract
(begin
  (print "Time-Locked Deals contract deployed")
)