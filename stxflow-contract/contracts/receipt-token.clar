;; Tokenized Receipts Smart Contract
;; Purpose: Make order history verifiable and tamper-proof
;; Each purchase mints a "receipt token" with metadata

;; Define the NFT
(define-non-fungible-token receipt-token uint)

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_OWNER_ONLY (err u100))
(define-constant ERR_NOT_TOKEN_OWNER (err u101))
(define-constant ERR_TOKEN_NOT_FOUND (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_MERCHANT_NOT_AUTHORIZED (err u104))
(define-constant ERR_TOKEN_ALREADY_EXISTS (err u105))

;; Data Variables
(define-data-var last-token-id uint u0)
(define-data-var contract-uri (string-ascii 256) "https://api.receipts.com/metadata/")

;; Data Maps
(define-map token-metadata
  uint
  {
    product-name: (string-ascii 100),
    product-category: (string-ascii 50),
    merchant: principal,
    buyer: principal,
    amount: uint,
    currency: (string-ascii 10),
    purchase-timestamp: uint,
    order-id: (string-ascii 50),
    warranty-expires: (optional uint),
    metadata-uri: (optional (string-ascii 256))
  }
)

(define-map authorized-merchants principal bool)
(define-map merchant-info
  principal
  {
    name: (string-ascii 100),
    category: (string-ascii 50),
    verified: bool,
    registration-date: uint
  }
)

(define-map buyer-receipts principal (list 1000 uint))
(define-map merchant-receipts principal (list 1000 uint))

;; Authorization Functions
(define-public (authorize-merchant (merchant principal) (name (string-ascii 100)) (category (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
    (map-set authorized-merchants merchant true)
    (map-set merchant-info merchant {
      name: name,
      category: category,
      verified: true,
      registration-date: stacks-block-height
    })
    (ok true)
  )
)

(define-public (revoke-merchant (merchant principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
    (map-delete authorized-merchants merchant)
    (ok true)
  )
)

;; Core Receipt Minting Function
(define-public (mint-receipt
  (buyer principal)
  (product-name (string-ascii 100))
  (product-category (string-ascii 50))
  (amount uint)
  (currency (string-ascii 10))
  (order-id (string-ascii 50))
  (warranty-months (optional uint))
  (metadata-uri (optional (string-ascii 256)))
)
  (let
    (
      (token-id (+ (var-get last-token-id) u1))
      (current-height stacks-block-height)
      (warranty-expires (match warranty-months
        months (some (+ current-height (* months u4320))) ;; Approximate blocks per month
        none
      ))
    )
    ;; Check if merchant is authorized
    (asserts! (default-to false (map-get? authorized-merchants tx-sender)) ERR_MERCHANT_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    ;; Mint the NFT to the buyer
    (try! (nft-mint? receipt-token token-id buyer))
    
    ;; Store receipt metadata
    (map-set token-metadata token-id {
      product-name: product-name,
      product-category: product-category,
      merchant: tx-sender,
      buyer: buyer,
      amount: amount,
      currency: currency,
      purchase-timestamp: current-height,
      order-id: order-id,
      warranty-expires: warranty-expires,
      metadata-uri: metadata-uri
    })
    
    ;; Update buyer's receipt list
    (map-set buyer-receipts buyer 
      (unwrap-panic (as-max-len? 
        (append (default-to (list) (map-get? buyer-receipts buyer)) token-id) 
        u1000
      ))
    )
    
    ;; Update merchant's receipt list
    (map-set merchant-receipts tx-sender
      (unwrap-panic (as-max-len?
        (append (default-to (list) (map-get? merchant-receipts tx-sender)) token-id)
        u1000
      ))
    )
    
    ;; Update last token ID
    (var-set last-token-id token-id)
    
    (ok token-id)
  )
)

;; Transfer function (for resale with receipt)
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR_NOT_TOKEN_OWNER)
    (try! (nft-transfer? receipt-token token-id sender recipient))
    
    ;; Update buyer receipts for new owner
    (map-set buyer-receipts recipient
      (unwrap-panic (as-max-len?
        (append (default-to (list) (map-get? buyer-receipts recipient)) token-id)
        u1000
      ))
    )
    
    (ok true)
  )
)

;; Read-only functions for verification and auditing
(define-read-only (get-receipt-metadata (token-id uint))
  (map-get? token-metadata token-id)
)

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? receipt-token token-id))
)

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id))
)

(define-read-only (get-buyer-receipts (buyer principal))
  (default-to (list) (map-get? buyer-receipts buyer))
)

(define-read-only (get-merchant-receipts (merchant principal))
  (default-to (list) (map-get? merchant-receipts merchant))
)

(define-read-only (is-merchant-authorized (merchant principal))
  (default-to false (map-get? authorized-merchants merchant))
)

(define-read-only (get-merchant-info (merchant principal))
  (map-get? merchant-info merchant)
)

;; Warranty verification - SECURITY: Uses current block height for comparison
(define-read-only (is-warranty-valid (token-id uint))
  (match (map-get? token-metadata token-id)
    receipt-data (match (get warranty-expires receipt-data)
      expiry (ok (< stacks-block-height expiry))
      (ok false)
    )
    (err ERR_TOKEN_NOT_FOUND)
  )
)

;; Audit functions
(define-read-only (verify-purchase 
  (token-id uint) 
  (expected-buyer principal) 
  (expected-merchant principal)
  (expected-amount uint)
)
  (match (map-get? token-metadata token-id)
    receipt-data (ok {
      buyer-match: (is-eq (get buyer receipt-data) expected-buyer),
      merchant-match: (is-eq (get merchant receipt-data) expected-merchant),
      amount-match: (is-eq (get amount receipt-data) expected-amount),
      purchase-timestamp: (get purchase-timestamp receipt-data)
    })
    (err ERR_TOKEN_NOT_FOUND)
  )
)

;; Get receipt by order ID (for merchant lookup) - FIXED
(define-read-only (get-receipt-by-order (merchant principal) (target-order-id (string-ascii 50)))
  (let
    (
      (merchant-tokens (get-merchant-receipts merchant))
    )
    (fold check-order-match-fold merchant-tokens {target-order: target-order-id, found: none})
  )
)

;; Helper function for fold operation - FIXED: All match arms return same type
(define-private (check-order-match-fold 
  (token-id uint) 
  (accumulator {target-order: (string-ascii 50), found: (optional uint)})
)
  (let
    (
      (target-order-id (get target-order accumulator))
      (current-found (get found accumulator))
    )
    (match current-found
      existing {target-order: target-order-id, found: (some existing)}
      (match (map-get? token-metadata token-id)
        receipt-data (if (is-eq (get order-id receipt-data) target-order-id)
          {target-order: target-order-id, found: (some token-id)}
          {target-order: target-order-id, found: none}
        )
        {target-order: target-order-id, found: none}
      )
    )
  )
)

;; Helper function to extract just the found token ID
(define-read-only (get-receipt-by-order-result (merchant principal) (target-order-id (string-ascii 50)))
  (get found (get-receipt-by-order merchant target-order-id))
)

;; Additional security function: Get current block height for external verification
(define-read-only (get-current-block-height)
  (ok stacks-block-height)
)

;; Enhanced warranty check with specific expiry block
(define-read-only (get-warranty-expiry-block (token-id uint))
  (match (map-get? token-metadata token-id)
    receipt-data (ok (get warranty-expires receipt-data))
    (err ERR_TOKEN_NOT_FOUND)
  )
)

;; URI functions for metadata
(define-read-only (get-token-uri (token-id uint))
  (ok (some (concat (var-get contract-uri) (uint-to-string token-id))))
)

(define-public (set-contract-uri (new-uri (string-ascii 256)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
    (var-set contract-uri new-uri)
    (ok true)
  )
)

;; Helper function to convert uint to string (more robust)
(define-private (uint-to-string (value uint))
  (if (is-eq value u0)
    "0"
    (if (< value u10)
      (unwrap-panic (element-at "0123456789" value))
      (if (< value u100)
        (concat 
          (unwrap-panic (element-at "0123456789" (/ value u10)))
          (unwrap-panic (element-at "0123456789" (mod value u10)))
        )
        "large-number" ;; Placeholder for numbers >= 100
      )
    )
  )
)