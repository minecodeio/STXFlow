;; =============================================================================
;; LOYALTY & REWARDS SYSTEM SMART CONTRACT
;; =============================================================================
;; Purpose: Incentivize user behavior through NFT/token rewards and tiered benefits
;; Features: Purchase tracking, tier-based rewards, NFT minting, referral system

;; =============================================================================
;; CONSTANTS & ERROR CODES
;; =============================================================================

(define-constant CONTRACT_OWNER tx-sender)

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_USER_NOT_FOUND (err u103))
(define-constant ERR_INVALID_TIER (err u104))
(define-constant ERR_REWARD_NOT_AVAILABLE (err u105))
(define-constant ERR_ALREADY_REFERRED (err u106))
(define-constant ERR_SELF_REFERRAL (err u107))
(define-constant ERR_NFT_MINT_FAILED (err u108))

;; Tier constants
(define-constant BRONZE_TIER u1)
(define-constant SILVER_TIER u2)
(define-constant GOLD_TIER u3)
(define-constant PLATINUM_TIER u4)

;; Spending thresholds (in microSTX)
(define-constant BRONZE_THRESHOLD u0)
(define-constant SILVER_THRESHOLD u5000000)   ;; 5 STX
(define-constant GOLD_THRESHOLD u20000000)    ;; 20 STX
(define-constant PLATINUM_THRESHOLD u50000000) ;; 50 STX

;; Point multipliers (basis points: 100 = 1%)
(define-constant BRONZE_MULTIPLIER u50)   ;; 0.5%
(define-constant SILVER_MULTIPLIER u100)  ;; 1%
(define-constant GOLD_MULTIPLIER u200)    ;; 2%
(define-constant PLATINUM_MULTIPLIER u300) ;; 3%

;; Referral bonus
(define-constant REFERRAL_BONUS u1000)

;; =============================================================================
;; DATA VARIABLES
;; =============================================================================

(define-data-var total-users uint u0)
(define-data-var total-purchases uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var nft-counter uint u0)
(define-data-var reward-counter uint u0)

;; =============================================================================
;; DATA MAPS
;; =============================================================================

;; User profile data
(define-map users principal {
    total-spent: uint,
    loyalty-points: uint,
    tier: uint,
    referral-count: uint,
    last-purchase-block: uint,
    nft-count: uint,
    is-active: bool
})

;; Purchase history
(define-map purchases uint {
    user: principal,
    amount: uint,
    points-earned: uint,
    block-height: uint,
    tier-at-purchase: uint
})

;; Referral tracking
(define-map referrals principal principal) ;; referee -> referrer

;; Tier configuration
(define-map tier-config uint {
    name: (string-ascii 20),
    threshold: uint,
    multiplier: uint,
    nft-eligible: bool,
    special-perks: bool
})

;; NFT registry
(define-map loyalty-nfts uint {
    owner: principal,
    tier: uint,
    points-value: uint,
    minted-at: uint,
    is-active: bool
})

;; Reward catalog
(define-map reward-catalog uint {
    name: (string-ascii 50),
    description: (string-ascii 200),
    cost: uint,
    tier-required: uint,
    is-available: bool,
    total-redeemed: uint
})

;; User reward redemptions
(define-map user-redemptions { user: principal, reward-id: uint } {
    redeemed-at: uint,
    points-spent: uint
})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

;; Initialize tier configurations
(map-set tier-config BRONZE_TIER {
    name: "Bronze",
    threshold: BRONZE_THRESHOLD,
    multiplier: BRONZE_MULTIPLIER,
    nft-eligible: false,
    special-perks: false
})

(map-set tier-config SILVER_TIER {
    name: "Silver",
    threshold: SILVER_THRESHOLD,
    multiplier: SILVER_MULTIPLIER,
    nft-eligible: true,
    special-perks: false
})

(map-set tier-config GOLD_TIER {
    name: "Gold",
    threshold: GOLD_THRESHOLD,
    multiplier: GOLD_MULTIPLIER,
    nft-eligible: true,
    special-perks: true
})

(map-set tier-config PLATINUM_TIER {
    name: "Platinum",
    threshold: PLATINUM_THRESHOLD,
    multiplier: PLATINUM_MULTIPLIER,
    nft-eligible: true,
    special-perks: true
})

;; Initialize reward catalog
(map-set reward-catalog u1 {
    name: "5% Discount Coupon",
    description: "Get 5% off your next purchase",
    cost: u500,
    tier-required: BRONZE_TIER,
    is-available: true,
    total-redeemed: u0
})

(map-set reward-catalog u2 {
    name: "Free Shipping",
    description: "Free shipping on your next order",
    cost: u1000,
    tier-required: SILVER_TIER,
    is-available: true,
    total-redeemed: u0
})

(map-set reward-catalog u3 {
    name: "Exclusive NFT",
    description: "Limited edition loyalty NFT",
    cost: u2500,
    tier-required: GOLD_TIER,
    is-available: true,
    total-redeemed: u0
})

(map-set reward-catalog u4 {
    name: "VIP Experience",
    description: "Exclusive VIP customer experience",
    cost: u5000,
    tier-required: PLATINUM_TIER,
    is-available: true,
    total-redeemed: u0
})

;; Set initial reward counter
(var-set reward-counter u4)

;; =============================================================================
;; HELPER FUNCTIONS
;; =============================================================================

(define-private (calculate-tier (total-spent uint))
    (if (>= total-spent PLATINUM_THRESHOLD)
        PLATINUM_TIER
        (if (>= total-spent GOLD_THRESHOLD)
            GOLD_TIER
            (if (>= total-spent SILVER_THRESHOLD)
                SILVER_TIER
                BRONZE_TIER))))

(define-private (calculate-points (amount uint) (tier uint))
    (let ((config (unwrap-panic (map-get? tier-config tier))))
        (/ (* amount (get multiplier config)) u10000)))

(define-private (is-tier-upgrade (old-tier uint) (new-tier uint))
    (> new-tier old-tier))

(define-private (get-tier-config (tier uint))
    (map-get? tier-config tier))

;; =============================================================================
;; PUBLIC FUNCTIONS
;; =============================================================================

;; Register a new user in the loyalty system
(define-public (register-user)
    (let ((user tx-sender))
        (match (map-get? users user)
            existing-user (ok false) ;; User already exists
            (begin
                (map-set users user {
                    total-spent: u0,
                    loyalty-points: u0,
                    tier: BRONZE_TIER,
                    referral-count: u0,
                    last-purchase-block: stacks-block-height,
                    nft-count: u0,
                    is-active: true
                })
                (var-set total-users (+ (var-get total-users) u1))
                (ok true)))))

;; Record a purchase and award loyalty points
(define-public (record-purchase (amount uint))
    (let (
        (user tx-sender)
        (user-data (unwrap! (map-get? users user) ERR_USER_NOT_FOUND))
        (old-tier (get tier user-data))
        (new-total-spent (+ (get total-spent user-data) amount))
        (new-tier (calculate-tier new-total-spent))
        (points-earned (calculate-points amount new-tier))
        (new-points (+ (get loyalty-points user-data) points-earned))
        (purchase-id (+ (var-get total-purchases) u1))
    )
        ;; Validate input
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (get is-active user-data) ERR_UNAUTHORIZED)
        
        ;; Record the purchase
        (map-set purchases purchase-id {
            user: user,
            amount: amount,
            points-earned: points-earned,
            block-height: stacks-block-height,
            tier-at-purchase: new-tier
        })
        
        ;; Update user data
        (map-set users user (merge user-data {
            total-spent: new-total-spent,
            loyalty-points: new-points,
            tier: new-tier,
            last-purchase-block: stacks-block-height
        }))
        
        ;; Update counters
        (var-set total-purchases purchase-id)
        (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) points-earned))
        
        ;; Handle tier upgrade
        (if (is-tier-upgrade old-tier new-tier)
            (begin
                (try! (mint-tier-upgrade-nft user new-tier))
                (ok { points-earned: points-earned, tier-upgraded: true, new-tier: new-tier }))
            (ok { points-earned: points-earned, tier-upgraded: false, new-tier: new-tier }))))

;; Handle referral system
(define-public (record-referral (referee principal))
    (let (
        (referrer tx-sender)
        (referrer-data (unwrap! (map-get? users referrer) ERR_USER_NOT_FOUND))
        (referee-data (unwrap! (map-get? users referee) ERR_USER_NOT_FOUND))
    )
        ;; Validation
        (asserts! (not (is-eq referrer referee)) ERR_SELF_REFERRAL)
        (asserts! (is-none (map-get? referrals referee)) ERR_ALREADY_REFERRED)
        (asserts! (get is-active referrer-data) ERR_UNAUTHORIZED)
        (asserts! (get is-active referee-data) ERR_UNAUTHORIZED)
        
        ;; Record the referral
        (map-set referrals referee referrer)
        
        ;; Calculate and award bonus points
        (let (
            (bonus-points REFERRAL_BONUS)
            (new-referral-count (+ (get referral-count referrer-data) u1))
            (new-points (+ (get loyalty-points referrer-data) bonus-points))
        )
            (map-set users referrer (merge referrer-data {
                referral-count: new-referral-count,
                loyalty-points: new-points
            }))
            
            (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) bonus-points))
            (ok { bonus-points: bonus-points, total-referrals: new-referral-count }))))

;; Mint NFT for tier upgrades
(define-private (mint-tier-upgrade-nft (recipient principal) (tier uint))
    (let (
        (nft-id (+ (var-get nft-counter) u1))
        (tier-info (unwrap! (get-tier-config tier) ERR_INVALID_TIER))
        (user-data (unwrap! (map-get? users recipient) ERR_USER_NOT_FOUND))
        (points-value (* tier u500))
    )
        ;; Check if tier is eligible for NFTs
        (asserts! (get nft-eligible tier-info) ERR_NFT_MINT_FAILED)
        
        ;; Mint the NFT
        (map-set loyalty-nfts nft-id {
            owner: recipient,
            tier: tier,
            points-value: points-value,
            minted-at: stacks-block-height,
            is-active: true
        })
        
        ;; Update user NFT count
        (map-set users recipient (merge user-data {
            nft-count: (+ (get nft-count user-data) u1)
        }))
        
        ;; Update counter
        (var-set nft-counter nft-id)
        (ok nft-id)))

;; Redeem loyalty points for rewards
(define-public (redeem-reward (reward-id uint))
    (let (
        (user tx-sender)
        (user-data (unwrap! (map-get? users user) ERR_USER_NOT_FOUND))
        (reward (unwrap! (map-get? reward-catalog reward-id) ERR_REWARD_NOT_AVAILABLE))
        (redemption-key { user: user, reward-id: reward-id })
    )
        ;; Validation
        (asserts! (get is-available reward) ERR_REWARD_NOT_AVAILABLE)
        (asserts! (>= (get tier user-data) (get tier-required reward)) ERR_INVALID_TIER)
        (asserts! (>= (get loyalty-points user-data) (get cost reward)) ERR_INSUFFICIENT_BALANCE)
        (asserts! (get is-active user-data) ERR_UNAUTHORIZED)
        
        ;; Deduct points from user
        (map-set users user (merge user-data {
            loyalty-points: (- (get loyalty-points user-data) (get cost reward))
        }))
        
        ;; Record the redemption
        (map-set user-redemptions redemption-key {
            redeemed-at: stacks-block-height,
            points-spent: (get cost reward)
        })
        
        ;; Update reward statistics
        (map-set reward-catalog reward-id (merge reward {
            total-redeemed: (+ (get total-redeemed reward) u1)
        }))
        
        (ok { reward-id: reward-id, points-spent: (get cost reward) })))

;; =============================================================================
;; ADMIN FUNCTIONS
;; =============================================================================

;; Add new reward to catalog
(define-public (add-reward (name (string-ascii 50)) (description (string-ascii 200)) (cost uint) (tier-required uint))
    (let ((reward-id (+ (var-get reward-counter) u1)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> cost u0) ERR_INVALID_AMOUNT)
        (asserts! (<= tier-required PLATINUM_TIER) ERR_INVALID_TIER)
        
        (map-set reward-catalog reward-id {
            name: name,
            description: description,
            cost: cost,
            tier-required: tier-required,
            is-available: true,
            total-redeemed: u0
        })
        
        (var-set reward-counter reward-id)
        (ok reward-id)))

;; Toggle reward availability
(define-public (toggle-reward-availability (reward-id uint))
    (let ((reward (unwrap! (map-get? reward-catalog reward-id) ERR_REWARD_NOT_AVAILABLE)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        (map-set reward-catalog reward-id (merge reward {
            is-available: (not (get is-available reward))
        }))
        
        (ok (not (get is-available reward)))))

;; Deactivate user account
(define-public (deactivate-user (user principal))
    (let ((user-data (unwrap! (map-get? users user) ERR_USER_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        (map-set users user (merge user-data {
            is-active: false
        }))
        
        (ok true)))

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

(define-read-only (get-user-profile (user principal))
    (map-get? users user))

(define-read-only (get-user-tier (user principal))
    (match (map-get? users user)
        user-data (ok (get tier user-data))
        ERR_USER_NOT_FOUND))

(define-read-only (get-loyalty-points (user principal))
    (match (map-get? users user)
        user-data (ok (get loyalty-points user-data))
        ERR_USER_NOT_FOUND))

(define-read-only (get-tier-info (tier uint))
    (map-get? tier-config tier))

(define-read-only (get-purchase-info (purchase-id uint))
    (map-get? purchases purchase-id))

(define-read-only (get-nft-info (nft-id uint))
    (map-get? loyalty-nfts nft-id))

(define-read-only (get-reward-info (reward-id uint))
    (map-get? reward-catalog reward-id))

(define-read-only (get-user-redemption (user principal) (reward-id uint))
    (map-get? user-redemptions { user: user, reward-id: reward-id }))

(define-read-only (get-referrer (user principal))
    (map-get? referrals user))

(define-read-only (calculate-points-for-amount (user principal) (amount uint))
    (match (map-get? users user)
        user-data 
            (let ((tier (get tier user-data)))
                (ok (calculate-points amount tier)))
        ERR_USER_NOT_FOUND))

(define-read-only (get-next-tier-info (user principal))
    (match (map-get? users user)
        user-data
            (let (
                (current-tier (get tier user-data))
                (total-spent (get total-spent user-data))
            )
                (if (< current-tier PLATINUM_TIER)
                    (let ((next-tier (+ current-tier u1)))
                        (match (get-tier-config next-tier)
                            next-config
                                (ok {
                                    next-tier: next-tier,
                                    required-spending: (get threshold next-config),
                                    remaining-to-upgrade: (- (get threshold next-config) total-spent)
                                })
                            (err u0)))
                    (ok {
                        next-tier: current-tier,
                        required-spending: u0,
                        remaining-to-upgrade: u0
                    })))
        ERR_USER_NOT_FOUND))

(define-read-only (get-system-stats)
    {
        total-users: (var-get total-users),
        total-purchases: (var-get total-purchases),
        total-rewards-distributed: (var-get total-rewards-distributed),
        total-nfts-minted: (var-get nft-counter),
        total-rewards-available: (var-get reward-counter)
    })

;; Get available rewards for user's tier
(define-read-only (get-available-rewards-for-user (user principal))
    (match (map-get? users user)
        user-data
            (let ((user-tier (get tier user-data)))
                (ok {
                    user-tier: user-tier,
                    available-rewards: "Use get-reward-info for each reward ID 1-4"
                }))
        ERR_USER_NOT_FOUND))