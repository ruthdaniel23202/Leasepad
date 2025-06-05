(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_LEASE_NOT_FOUND (err u101))
(define-constant ERR_LEASE_ALREADY_EXISTS (err u102))
(define-constant ERR_LEASE_EXPIRED (err u103))
(define-constant ERR_LEASE_NOT_ACTIVE (err u104))
(define-constant ERR_ALREADY_SIGNED (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))
(define-constant ERR_PAYMENT_FAILED (err u107))
(define-constant ERR_LEASE_TERMINATED (err u108))

(define-data-var lease-counter uint u0)

(define-map leases
  uint
  {
    landlord: principal,
    tenant: principal,
    property-address: (string-ascii 256),
    monthly-rent: uint,
    security-deposit: uint,
    start-block: uint,
    end-block: uint,
    landlord-signed: bool,
    tenant-signed: bool,
    status: (string-ascii 20),
    created-at: uint
  }
)

(define-map lease-payments
  { lease-id: uint, payment-month: uint }
  {
    amount: uint,
    paid-at: uint,
    paid-by: principal
  }
)

(define-map user-leases
  principal
  (list 50 uint)
)

(define-public (create-lease
  (tenant principal)
  (property-address (string-ascii 256))
  (monthly-rent uint)
  (security-deposit uint)
  (duration-blocks uint))
  (let
    (
      (lease-id (+ (var-get lease-counter) u1))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height duration-blocks))
    )
    (asserts! (> monthly-rent u0) ERR_INVALID_AMOUNT)
    (asserts! (> security-deposit u0) ERR_INVALID_AMOUNT)
    (asserts! (> duration-blocks u0) ERR_INVALID_AMOUNT)
    
    (map-set leases lease-id
      {
        landlord: tx-sender,
        tenant: tenant,
        property-address: property-address,
        monthly-rent: monthly-rent,
        security-deposit: security-deposit,
        start-block: start-block,
        end-block: end-block,
        landlord-signed: true,
        tenant-signed: false,
        status: "pending",
        created-at: stacks-block-height
      }
    )
    
    (try! (update-user-leases tx-sender lease-id))
    (try! (update-user-leases tenant lease-id))
    (var-set lease-counter lease-id)
    (ok lease-id)
  )
)

(define-public (sign-lease (lease-id uint))
  (let
    (
      (lease (unwrap! (map-get? leases lease-id) ERR_LEASE_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get landlord lease)) 
                  (is-eq tx-sender (get tenant lease))) ERR_NOT_AUTHORIZED)
    
    (if (is-eq tx-sender (get landlord lease))
      (begin
        (asserts! (not (get landlord-signed lease)) ERR_ALREADY_SIGNED)
        (map-set leases lease-id (merge lease { landlord-signed: true }))
      )
      (begin
        (asserts! (not (get tenant-signed lease)) ERR_ALREADY_SIGNED)
        (map-set leases lease-id (merge lease { tenant-signed: true }))
      )
    )
    
    (let
      (
        (updated-lease (unwrap! (map-get? leases lease-id) ERR_LEASE_NOT_FOUND))
      )
      (if (and (get landlord-signed updated-lease) (get tenant-signed updated-lease))
        (map-set leases lease-id (merge updated-lease { status: "active" }))
        true
      )
    )
    (ok true)
  )
)

(define-public (pay-rent (lease-id uint) (payment-month uint))
  (let
    (
      (lease (unwrap! (map-get? leases lease-id) ERR_LEASE_NOT_FOUND))
    )
    (asserts! (is-eq (get status lease) "active") ERR_LEASE_NOT_ACTIVE)
    (asserts! (is-eq tx-sender (get tenant lease)) ERR_NOT_AUTHORIZED)
    (asserts! (< stacks-block-height (get end-block lease)) ERR_LEASE_EXPIRED)
    (asserts! (is-none (map-get? lease-payments { lease-id: lease-id, payment-month: payment-month })) ERR_ALREADY_SIGNED)
    
    (try! (stx-transfer? (get monthly-rent lease) tx-sender (get landlord lease)))
    
    (map-set lease-payments 
      { lease-id: lease-id, payment-month: payment-month }
      {
        amount: (get monthly-rent lease),
        paid-at: stacks-block-height,
        paid-by: tx-sender
      }
    )
    (ok true)
  )
)

(define-public (pay-security-deposit (lease-id uint))
  (let
    (
      (lease (unwrap! (map-get? leases lease-id) ERR_LEASE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get tenant lease)) ERR_NOT_AUTHORIZED)
    (asserts! (and (get landlord-signed lease) (get tenant-signed lease)) ERR_LEASE_NOT_ACTIVE)
    
    (try! (stx-transfer? (get security-deposit lease) tx-sender (get landlord lease)))
    (ok true)
  )
)

(define-public (terminate-lease (lease-id uint))
  (let
    (
      (lease (unwrap! (map-get? leases lease-id) ERR_LEASE_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get landlord lease)) 
                  (is-eq tx-sender (get tenant lease))) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status lease) "active") ERR_LEASE_NOT_ACTIVE)
    
    (map-set leases lease-id (merge lease { status: "terminated" }))
    (ok true)
  )
)

(define-read-only (get-lease (lease-id uint))
  (map-get? leases lease-id)
)

(define-read-only (get-payment (lease-id uint) (payment-month uint))
  (map-get? lease-payments { lease-id: lease-id, payment-month: payment-month })
)

(define-read-only (get-user-leases (user principal))
  (default-to (list) (map-get? user-leases user))
)

(define-read-only (get-lease-counter)
  (var-get lease-counter)
)

(define-read-only (is-lease-active (lease-id uint))
  (match (map-get? leases lease-id)
    lease (and 
            (is-eq (get status lease) "active")
            (< stacks-block-height (get end-block lease)))
    false
  )
)

(define-read-only (is-lease-expired (lease-id uint))
  (match (map-get? leases lease-id)
    lease (>= stacks-block-height (get end-block lease))
    false
  )
)

(define-read-only (get-lease-status (lease-id uint))
  (match (map-get? leases lease-id)
    lease (get status lease)
    "not-found"
  )
)

(define-private (update-user-leases (user principal) (lease-id uint))
  (let
    (
      (current-leases (default-to (list) (map-get? user-leases user)))
    )
    (map-set user-leases user (unwrap! (as-max-len? (append current-leases lease-id) u50) (err u999)))
    (ok true)
  )
)