(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-already-listed (err u104))
(define-constant err-not-listed (err u105))
(define-constant err-already-rated (err u106))
(define-constant err-invalid-rating (err u107))
(define-constant err-not-owner (err u108))
(define-constant err-auction-not-found (err u109))
(define-constant err-auction-ended (err u110))
(define-constant err-auction-active (err u111))
(define-constant err-bid-too-low (err u112))
(define-constant err-credit-retired (err u113))
(define-constant err-zero-retirement (err u114))

(define-non-fungible-token carbon-credit uint)

(define-map credit-data
    { credit-id: uint }
    {
        project-name: (string-ascii 50),
        location: (string-ascii 50),
        vintage-year: uint,
        quantity: uint,
        verification-standard: (string-ascii 20),
        price: uint,
        owner: principal,
        listed: bool,
    }
)

(define-map project-verifiers
    { verifier: principal }
    { active: bool }
)

(define-map credit-ratings
    {
        credit-id: uint,
        rater: principal,
    }
    {
        rating: uint,
        comment: (string-ascii 100),
    }
)

(define-map credit-reputation
    { credit-id: uint }
    {
        total-rating: uint,
        rating-count: uint,
        average-rating: uint,
    }
)

(define-map retired-credits
    { credit-id: uint }
    {
        retired-by: principal,
        retirement-block: uint,
        retirement-reason: (string-ascii 100),
        retired-quantity: uint,
    }
)

(define-data-var next-credit-id uint u1)
(define-data-var marketplace-fee uint u25)
(define-data-var next-auction-id uint u1)

(define-map auction-data
    { auction-id: uint }
    {
        credit-id: uint,
        seller: principal,
        starting-price: uint,
        current-bid: uint,
        highest-bidder: (optional principal),
        end-block: uint,
        active: bool,
    }
)

(define-public (register-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set project-verifiers { verifier: verifier } { active: true }))
    )
)

(define-public (remove-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set project-verifiers { verifier: verifier } { active: false }))
    )
)

(define-read-only (is-verifier (address principal))
    (default-to false
        (get active (map-get? project-verifiers { verifier: address }))
    )
)

(define-public (mint-carbon-credits
        (project-name (string-ascii 50))
        (location (string-ascii 50))
        (vintage-year uint)
        (quantity uint)
        (verification-standard (string-ascii 20))
        (price uint)
    )
    (let ((credit-id (var-get next-credit-id)))
        (asserts! (is-verifier tx-sender) err-unauthorized)
        (asserts! (> quantity u0) err-invalid-amount)
        (try! (nft-mint? carbon-credit credit-id tx-sender))
        (map-set credit-data { credit-id: credit-id } {
            project-name: project-name,
            location: location,
            vintage-year: vintage-year,
            quantity: quantity,
            verification-standard: verification-standard,
            price: price,
            owner: tx-sender,
            listed: false,
        })
        (var-set next-credit-id (+ credit-id u1))
        (ok credit-id)
    )
)

(define-public (list-credits
        (credit-id uint)
        (new-price uint)
    )
    (let (
            (credit (unwrap! (map-get? credit-data { credit-id: credit-id })
                err-not-found
            ))
            (owner (get owner credit))
        )
        (asserts! (is-eq tx-sender owner) err-unauthorized)
        (asserts! (not (get listed credit)) err-already-listed)
        (ok (map-set credit-data { credit-id: credit-id }
            (merge credit {
                price: new-price,
                listed: true,
            })
        ))
    )
)

(define-public (unlist-credits (credit-id uint))
    (let (
            (credit (unwrap! (map-get? credit-data { credit-id: credit-id })
                err-not-found
            ))
            (owner (get owner credit))
        )
        (asserts! (is-eq tx-sender owner) err-unauthorized)
        (asserts! (get listed credit) err-not-listed)
        (ok (map-set credit-data { credit-id: credit-id }
            (merge credit { listed: false })
        ))
    )
)

(define-public (buy-credits (credit-id uint))
    (let (
            (credit (unwrap! (map-get? credit-data { credit-id: credit-id })
                err-not-found
            ))
            (price (get price credit))
            (seller (get owner credit))
        )
        (asserts! (get listed credit) err-not-listed)
        (asserts! (is-none (map-get? retired-credits { credit-id: credit-id }))
            err-credit-retired
        )
        (asserts! (>= (stx-get-balance tx-sender) price) err-invalid-amount)
        (try! (stx-transfer? price tx-sender seller))
        (try! (nft-transfer? carbon-credit credit-id seller tx-sender))
        (ok (map-set credit-data { credit-id: credit-id }
            (merge credit {
                owner: tx-sender,
                listed: false,
            })
        ))
    )
)

(define-read-only (get-credit-data (credit-id uint))
    (map-get? credit-data { credit-id: credit-id })
)

(define-read-only (get-credit-owner (credit-id uint))
    (ok (get owner
        (unwrap! (map-get? credit-data { credit-id: credit-id }) err-not-found)
    ))
)

(define-public (transfer-credits
        (credit-id uint)
        (recipient principal)
    )
    (let (
            (credit (unwrap! (map-get? credit-data { credit-id: credit-id })
                err-not-found
            ))
            (owner (get owner credit))
        )
        (asserts! (is-eq tx-sender owner) err-unauthorized)
        (asserts! (is-none (map-get? retired-credits { credit-id: credit-id }))
            err-credit-retired
        )
        (try! (nft-transfer? carbon-credit credit-id tx-sender recipient))
        (ok (map-set credit-data { credit-id: credit-id }
            (merge credit {
                owner: recipient,
                listed: false,
            })
        ))
    )
)

(define-public (rate-credit
        (credit-id uint)
        (rating uint)
        (comment (string-ascii 100))
    )
    (let (
            (credit (unwrap! (map-get? credit-data { credit-id: credit-id })
                err-not-found
            ))
            (existing-rating (map-get? credit-ratings {
                credit-id: credit-id,
                rater: tx-sender,
            }))
            (current-reputation (default-to {
                total-rating: u0,
                rating-count: u0,
                average-rating: u0,
            }
                (map-get? credit-reputation { credit-id: credit-id })
            ))
        )
        (asserts! (not (is-eq tx-sender (get owner credit))) err-not-owner)
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
        (asserts! (is-none existing-rating) err-already-rated)
        (map-set credit-ratings {
            credit-id: credit-id,
            rater: tx-sender,
        } {
            rating: rating,
            comment: comment,
        })
        (let (
                (new-total-rating (+ (get total-rating current-reputation) rating))
                (new-rating-count (+ (get rating-count current-reputation) u1))
                (new-average-rating (/ new-total-rating new-rating-count))
            )
            (map-set credit-reputation { credit-id: credit-id } {
                total-rating: new-total-rating,
                rating-count: new-rating-count,
                average-rating: new-average-rating,
            })
        )
        (ok true)
    )
)

(define-read-only (get-credit-reputation (credit-id uint))
    (map-get? credit-reputation { credit-id: credit-id })
)

(define-read-only (get-credit-rating
        (credit-id uint)
        (rater principal)
    )
    (map-get? credit-ratings {
        credit-id: credit-id,
        rater: rater,
    })
)

(define-public (create-auction
        (credit-id uint)
        (starting-price uint)
        (duration uint)
    )
    (let (
            (credit (unwrap! (map-get? credit-data { credit-id: credit-id })
                err-not-found
            ))
            (owner (get owner credit))
            (auction-id (var-get next-auction-id))
            (end-block (+ burn-block-height duration))
        )
        (asserts! (is-eq tx-sender owner) err-unauthorized)
        (asserts! (not (get listed credit)) err-already-listed)
        (asserts! (> starting-price u0) err-invalid-amount)
        (map-set auction-data { auction-id: auction-id } {
            credit-id: credit-id,
            seller: tx-sender,
            starting-price: starting-price,
            current-bid: starting-price,
            highest-bidder: none,
            end-block: end-block,
            active: true,
        })
        (var-set next-auction-id (+ auction-id u1))
        (ok auction-id)
    )
)

(define-public (place-bid
        (auction-id uint)
        (bid-amount uint)
    )
    (let (
            (auction (unwrap! (map-get? auction-data { auction-id: auction-id })
                err-auction-not-found
            ))
            (current-bid (get current-bid auction))
            (previous-bidder (get highest-bidder auction))
        )
        (asserts! (get active auction) err-auction-ended)
        (asserts! (< burn-block-height (get end-block auction)) err-auction-ended)
        (asserts! (> bid-amount current-bid) err-bid-too-low)
        (asserts! (>= (stx-get-balance tx-sender) bid-amount) err-invalid-amount)
        (match previous-bidder
            bidder (try! (stx-transfer? current-bid contract-caller bidder))
            true
        )
        (try! (stx-transfer? bid-amount tx-sender contract-caller))
        (map-set auction-data { auction-id: auction-id }
            (merge auction {
                current-bid: bid-amount,
                highest-bidder: (some tx-sender),
            })
        )
        (ok true)
    )
)

(define-public (finalize-auction (auction-id uint))
    (let (
            (auction (unwrap! (map-get? auction-data { auction-id: auction-id })
                err-auction-not-found
            ))
            (credit-id (get credit-id auction))
            (seller (get seller auction))
            (winner (get highest-bidder auction))
            (final-price (get current-bid auction))
        )
        (asserts! (get active auction) err-auction-ended)
        (asserts! (>= burn-block-height (get end-block auction))
            err-auction-active
        )
        (match winner
            bidder (begin
                (try! (stx-transfer? final-price contract-caller seller))
                (try! (nft-transfer? carbon-credit credit-id seller bidder))
                (map-set credit-data { credit-id: credit-id }
                    (merge
                        (unwrap-panic (map-get? credit-data { credit-id: credit-id })) {
                        owner: bidder,
                        listed: false,
                    })
                )
            )
            (begin
                (try! (stx-transfer? final-price contract-caller seller))
            )
        )
        (map-set auction-data { auction-id: auction-id }
            (merge auction { active: false })
        )
        (ok true)
    )
)

(define-read-only (get-auction-data (auction-id uint))
    (map-get? auction-data { auction-id: auction-id })
)

(define-public (retire-credits
        (credit-id uint)
        (retirement-reason (string-ascii 100))
    )
    (let (
            (credit (unwrap! (map-get? credit-data { credit-id: credit-id })
                err-not-found
            ))
            (owner (get owner credit))
            (quantity (get quantity credit))
        )
        (asserts! (is-eq tx-sender owner) err-unauthorized)
        (asserts! (is-none (map-get? retired-credits { credit-id: credit-id }))
            err-credit-retired
        )
        (asserts! (> quantity u0) err-zero-retirement)
        (map-set retired-credits { credit-id: credit-id } {
            retired-by: tx-sender,
            retirement-block: burn-block-height,
            retirement-reason: retirement-reason,
            retired-quantity: quantity,
        })
        (ok true)
    )
)

(define-read-only (get-retirement-data (credit-id uint))
    (map-get? retired-credits { credit-id: credit-id })
)

(define-read-only (is-credit-retired (credit-id uint))
    (is-some (map-get? retired-credits { credit-id: credit-id }))
)
