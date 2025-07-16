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

(define-data-var next-credit-id uint u1)
(define-data-var marketplace-fee uint u25)

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
        (asserts! (is-eq (stx-get-balance tx-sender) price) err-invalid-amount)
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
