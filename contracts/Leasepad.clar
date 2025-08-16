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
(define-constant ERR_DISPUTE_NOT_FOUND (err u109))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u110))
(define-constant ERR_DISPUTE_CLOSED (err u111))
(define-constant ERR_INVALID_DISPUTE_TYPE (err u112))
(define-constant ERR_EVIDENCE_LIMIT_EXCEEDED (err u113))
(define-constant ERR_ARBITRATOR_NOT_AUTHORIZED (err u114))
(define-constant ERR_DISPUTE_NOT_PENDING (err u115))
(define-constant ERR_DISPUTE_ALREADY_VOTED (err u116))
(define-constant ERR_INVALID_VOTE (err u117))
(define-constant ERR_AMENDMENT_NOT_FOUND (err u118))
(define-constant ERR_AMENDMENT_ALREADY_EXISTS (err u119))
(define-constant ERR_AMENDMENT_EXPIRED (err u120))
(define-constant ERR_AMENDMENT_ALREADY_APPROVED (err u121))
(define-constant ERR_AMENDMENT_ALREADY_REJECTED (err u122))
(define-constant ERR_AMENDMENT_NOT_PENDING (err u123))
(define-constant ERR_INVALID_AMENDMENT_TYPE (err u124))
(define-constant ERR_INVALID_EFFECTIVE_DATE (err u125))
(define-constant ERR_AMENDMENT_LIMIT_EXCEEDED (err u126))

(define-data-var lease-counter uint u0)
(define-data-var dispute-counter uint u0)
(define-data-var amendment-counter uint u0)
(define-data-var arbitrator principal CONTRACT_OWNER)

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

(define-map disputes
  uint
  {
    lease-id: uint,
    dispute-type: (string-ascii 50),
    complainant: principal,
    defendant: principal,
    description: (string-ascii 500),
    status: (string-ascii 20),
    created-at: uint,
    resolved-at: (optional uint),
    resolution: (optional (string-ascii 500)),
    arbitrator-vote: (optional (string-ascii 20)),
    complainant-vote: (optional (string-ascii 20)),
    defendant-vote: (optional (string-ascii 20))
  }
)

(define-map dispute-evidence
  { dispute-id: uint, evidence-id: uint }
  {
    submitter: principal,
    evidence-type: (string-ascii 30),
    evidence-hash: (string-ascii 64),
    description: (string-ascii 300),
    submitted-at: uint
  }
)

(define-map dispute-evidence-counter
  uint
  uint
)

(define-map dispute-votes
  { dispute-id: uint, voter: principal }
  {
    vote: (string-ascii 20),
    reasoning: (string-ascii 300),
    voted-at: uint
  }
)

(define-map lease-amendments
  uint
  {
    lease-id: uint,
    amendment-type: (string-ascii 30),
    proposer: principal,
    proposed-changes: (string-ascii 500),
    new-monthly-rent: (optional uint),
    new-end-block: (optional uint),
    new-terms: (optional (string-ascii 300)),
    effective-date: uint,
    expiry-date: uint,
    status: (string-ascii 20),
    landlord-approved: bool,
    tenant-approved: bool,
    proposed-at: uint,
    approved-at: (optional uint),
    rejection-reason: (optional (string-ascii 200))
  }
)

(define-map amendment-approval-history
  { amendment-id: uint, approver: principal }
  {
    approval-status: (string-ascii 15),
    approval-date: uint,
    notes: (string-ascii 300)
  }
)

(define-map lease-amendment-count
  uint
  uint
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

(define-public (file-dispute
  (lease-id uint)
  (dispute-type (string-ascii 50))
  (description (string-ascii 500)))
  (let
    (
      (lease (unwrap! (map-get? leases lease-id) ERR_LEASE_NOT_FOUND))
      (dispute-id (+ (var-get dispute-counter) u1))
      (complainant tx-sender)
      (defendant (if (is-eq tx-sender (get landlord lease)) 
                    (get tenant lease) 
                    (get landlord lease)))
    )
    (asserts! (or (is-eq tx-sender (get landlord lease)) 
                  (is-eq tx-sender (get tenant lease))) ERR_NOT_AUTHORIZED)
    (asserts! (or (is-eq (get status lease) "active") 
                  (is-eq (get status lease) "terminated")) ERR_LEASE_NOT_ACTIVE)
    (asserts! (or (is-eq dispute-type "rent-payment") 
                  (is-eq dispute-type "property-damage") 
                  (is-eq dispute-type "lease-violation") 
                  (is-eq dispute-type "security-deposit") 
                  (is-eq dispute-type "maintenance-issue") 
                  (is-eq dispute-type "early-termination") 
                  (is-eq dispute-type "other")) ERR_INVALID_DISPUTE_TYPE)

    
    (map-set disputes dispute-id
      {
        lease-id: lease-id,
        dispute-type: dispute-type,
        complainant: complainant,
        defendant: defendant,
        description: description,
        status: "pending",
        created-at: stacks-block-height,
        resolved-at: none,
        resolution: none,
        arbitrator-vote: none,
        complainant-vote: none,
        defendant-vote: none
      }
    )
    
    (map-set dispute-evidence-counter dispute-id u0)
    (var-set dispute-counter dispute-id)
    (ok dispute-id)
  )
)

(define-public (submit-evidence
  (dispute-id uint)
  (evidence-type (string-ascii 30))
  (evidence-hash (string-ascii 64))
  (description (string-ascii 300)))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
      (evidence-counter (default-to u0 (map-get? dispute-evidence-counter dispute-id)))
      (evidence-id (+ evidence-counter u1))
    )
    (asserts! (or (is-eq tx-sender (get complainant dispute)) 
                  (is-eq tx-sender (get defendant dispute))) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status dispute) "pending") ERR_DISPUTE_CLOSED)
    (asserts! (< evidence-counter u10) ERR_EVIDENCE_LIMIT_EXCEEDED)
    (asserts! (or (is-eq evidence-type "document") 
                  (is-eq evidence-type "photo") 
                  (is-eq evidence-type "video") 
                  (is-eq evidence-type "receipt") 
                  (is-eq evidence-type "communication") 
                  (is-eq evidence-type "inspection-report") 
                  (is-eq evidence-type "other")) ERR_INVALID_DISPUTE_TYPE)
    
    (map-set dispute-evidence 
      { dispute-id: dispute-id, evidence-id: evidence-id }
      {
        submitter: tx-sender,
        evidence-type: evidence-type,
        evidence-hash: evidence-hash,
        description: description,
        submitted-at: stacks-block-height
      }
    )
    
    (map-set dispute-evidence-counter dispute-id evidence-id)
    (ok evidence-id)
  )
)

(define-public (vote-on-dispute
  (dispute-id uint)
  (vote (string-ascii 20))
  (reasoning (string-ascii 300)))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
    )
    (asserts! (is-eq (get status dispute) "pending") ERR_DISPUTE_NOT_PENDING)
    (asserts! (or (is-eq tx-sender (get complainant dispute)) 
                  (is-eq tx-sender (get defendant dispute)) 
                  (is-eq tx-sender (var-get arbitrator))) ERR_NOT_AUTHORIZED)
    (asserts! (or (is-eq vote "favor-complainant") 
                  (is-eq vote "favor-defendant") 
                  (is-eq vote "neutral") 
                  (is-eq vote "insufficient-evidence")) ERR_INVALID_VOTE)
    (asserts! (is-none (map-get? dispute-votes { dispute-id: dispute-id, voter: tx-sender })) ERR_DISPUTE_ALREADY_VOTED)
    
    (map-set dispute-votes 
      { dispute-id: dispute-id, voter: tx-sender }
      {
        vote: vote,
        reasoning: reasoning,
        voted-at: stacks-block-height
      }
    )
    
    (let
      (
        (updated-dispute (if (is-eq tx-sender (get complainant dispute))
                           (merge dispute { complainant-vote: (some vote) })
                           (if (is-eq tx-sender (get defendant dispute))
                             (merge dispute { defendant-vote: (some vote) })
                             (merge dispute { arbitrator-vote: (some vote) }))))
      )
      (map-set disputes dispute-id updated-dispute)
      (try! (check-and-resolve-dispute dispute-id))
    )
    (ok true)
  )
)

(define-private (check-and-resolve-dispute (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
    )
    (if (and (is-some (get complainant-vote dispute)) 
             (is-some (get defendant-vote dispute))
             (is-some (get arbitrator-vote dispute)))
      (let
        (
          (complainant-vote (unwrap-panic (get complainant-vote dispute)))
          (defendant-vote (unwrap-panic (get defendant-vote dispute)))
          (arbitrator-vote (unwrap-panic (get arbitrator-vote dispute)))
        )
        (if (is-eq complainant-vote defendant-vote)
          (map-set disputes dispute-id 
            (merge dispute 
              { 
                status: "resolved",
                resolved-at: (some stacks-block-height),
                resolution: (some "Parties reached mutual agreement")
              }
            )
          )
          (map-set disputes dispute-id 
            (merge dispute 
              { 
                status: "resolved",
                resolved-at: (some stacks-block-height),
                resolution: (some "Resolved by arbitrator decision")
              }
            )
          )
        )
        (ok true)
      )
      (ok false)
    )
  )
)

(define-public (escalate-to-arbitrator (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get complainant dispute)) 
                  (is-eq tx-sender (get defendant dispute))) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status dispute) "pending") ERR_DISPUTE_CLOSED)
    (asserts! (or (is-some (get complainant-vote dispute)) 
                  (is-some (get defendant-vote dispute))) ERR_DISPUTE_NOT_PENDING)
    
    (map-set disputes dispute-id (merge dispute { status: "arbitration" }))
    (ok true)
  )
)

(define-public (resolve-dispute
  (dispute-id uint)
  (resolution (string-ascii 500)))
  (let
    (
      (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (var-get arbitrator)) ERR_ARBITRATOR_NOT_AUTHORIZED)
    (asserts! (or (is-eq (get status dispute) "pending") 
                  (is-eq (get status dispute) "arbitration")) ERR_DISPUTE_CLOSED)
    
    (map-set disputes dispute-id 
      (merge dispute 
        { 
          status: "resolved",
          resolved-at: (some stacks-block-height),
          resolution: (some resolution)
        }
      )
    )
    (ok true)
  )
)

(define-public (set-arbitrator (new-arbitrator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set arbitrator new-arbitrator)
    (ok true)
  )
)

(define-public (propose-lease-amendment
  (lease-id uint)
  (amendment-type (string-ascii 30))
  (proposed-changes (string-ascii 500))
  (new-monthly-rent (optional uint))
  (new-end-block (optional uint))
  (new-terms (optional (string-ascii 300)))
  (effective-date uint)
  (expiry-date uint))
  (let
    (
      (lease (unwrap! (map-get? leases lease-id) ERR_LEASE_NOT_FOUND))
      (amendment-id (+ (var-get amendment-counter) u1))
      (current-amendment-count (default-to u0 (map-get? lease-amendment-count lease-id)))
    )
    (asserts! (or (is-eq tx-sender (get landlord lease)) 
                  (is-eq tx-sender (get tenant lease))) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status lease) "active") ERR_LEASE_NOT_ACTIVE)
    (asserts! (or (is-eq amendment-type "rent-change") 
                  (is-eq amendment-type "term-extension") 
                  (is-eq amendment-type "clause-addition") 
                  (is-eq amendment-type "clause-modification") 
                  (is-eq amendment-type "security-deposit-change") 
                  (is-eq amendment-type "early-termination-clause") 
                  (is-eq amendment-type "maintenance-responsibility") 
                  (is-eq amendment-type "other")) ERR_INVALID_AMENDMENT_TYPE)
    (asserts! (> effective-date stacks-block-height) ERR_INVALID_EFFECTIVE_DATE)
    (asserts! (> expiry-date effective-date) ERR_INVALID_EFFECTIVE_DATE)
    (asserts! (< current-amendment-count u20) ERR_AMENDMENT_LIMIT_EXCEEDED)
    
    (if (is-some new-monthly-rent)
      (asserts! (> (unwrap-panic new-monthly-rent) u0) ERR_INVALID_AMOUNT)
      true
    )
    
    (if (is-some new-end-block)
      (asserts! (> (unwrap-panic new-end-block) (get end-block lease)) ERR_INVALID_EFFECTIVE_DATE)
      true
    )
    
    (map-set lease-amendments amendment-id
      {
        lease-id: lease-id,
        amendment-type: amendment-type,
        proposer: tx-sender,
        proposed-changes: proposed-changes,
        new-monthly-rent: new-monthly-rent,
        new-end-block: new-end-block,
        new-terms: new-terms,
        effective-date: effective-date,
        expiry-date: expiry-date,
        status: "pending",
        landlord-approved: (is-eq tx-sender (get landlord lease)),
        tenant-approved: (is-eq tx-sender (get tenant lease)),
        proposed-at: stacks-block-height,
        approved-at: none,
        rejection-reason: none
      }
    )
    
    (map-set lease-amendment-count lease-id (+ current-amendment-count u1))
    (var-set amendment-counter amendment-id)
    (ok amendment-id)
  )
)

(define-public (approve-amendment
  (amendment-id uint)
  (approval-notes (string-ascii 300)))
  (let
    (
      (amendment (unwrap! (map-get? lease-amendments amendment-id) ERR_AMENDMENT_NOT_FOUND))
      (lease (unwrap! (map-get? leases (get lease-id amendment)) ERR_LEASE_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get landlord lease)) 
                  (is-eq tx-sender (get tenant lease))) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status amendment) "pending") ERR_AMENDMENT_NOT_PENDING)
    (asserts! (< stacks-block-height (get expiry-date amendment)) ERR_AMENDMENT_EXPIRED)
    
    (let
      (
        (is-landlord (is-eq tx-sender (get landlord lease)))
        (updated-amendment (if is-landlord
                            (merge amendment { landlord-approved: true })
                            (merge amendment { tenant-approved: true })))
      )
      (asserts! (if is-landlord
                  (not (get landlord-approved amendment))
                  (not (get tenant-approved amendment))) ERR_AMENDMENT_ALREADY_APPROVED)
      
      (map-set amendment-approval-history 
        { amendment-id: amendment-id, approver: tx-sender }
        {
          approval-status: "approved",
          approval-date: stacks-block-height,
          notes: approval-notes
        }
      )
      
      (map-set lease-amendments amendment-id updated-amendment)
      
      (if (and (get landlord-approved updated-amendment) (get tenant-approved updated-amendment))
        (begin
          (try! (execute-amendment amendment-id))
          (ok true)
        )
        (ok true)
      )
    )
  )
)

(define-public (reject-amendment
  (amendment-id uint)
  (rejection-reason (string-ascii 200)))
  (let
    (
      (amendment (unwrap! (map-get? lease-amendments amendment-id) ERR_AMENDMENT_NOT_FOUND))
      (lease (unwrap! (map-get? leases (get lease-id amendment)) ERR_LEASE_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get landlord lease)) 
                  (is-eq tx-sender (get tenant lease))) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status amendment) "pending") ERR_AMENDMENT_NOT_PENDING)
    
    (map-set amendment-approval-history 
      { amendment-id: amendment-id, approver: tx-sender }
      {
        approval-status: "rejected",
        approval-date: stacks-block-height,
        notes: rejection-reason
      }
    )
    
    (map-set lease-amendments amendment-id 
      (merge amendment 
        { 
          status: "rejected",
          rejection-reason: (some rejection-reason)
        }
      )
    )
    (ok true)
  )
)

(define-public (execute-amendment (amendment-id uint))
  (let
    (
      (amendment (unwrap! (map-get? lease-amendments amendment-id) ERR_AMENDMENT_NOT_FOUND))
      (lease (unwrap! (map-get? leases (get lease-id amendment)) ERR_LEASE_NOT_FOUND))
    )
    (asserts! (and (get landlord-approved amendment) (get tenant-approved amendment)) ERR_AMENDMENT_NOT_PENDING)
    (asserts! (is-eq (get status amendment) "pending") ERR_AMENDMENT_NOT_PENDING)
    (asserts! (>= stacks-block-height (get effective-date amendment)) ERR_INVALID_EFFECTIVE_DATE)
    
    (let
      (
        (updated-lease (merge lease 
          {
            monthly-rent: (default-to (get monthly-rent lease) (get new-monthly-rent amendment)),
            end-block: (default-to (get end-block lease) (get new-end-block amendment))
          }
        ))
      )
      (map-set leases (get lease-id amendment) updated-lease)
      (map-set lease-amendments amendment-id 
        (merge amendment 
          { 
            status: "executed",
            approved-at: (some stacks-block-height)
          }
        )
      )
    )
    (ok true)
  )
)

(define-public (withdraw-amendment (amendment-id uint))
  (let
    (
      (amendment (unwrap! (map-get? lease-amendments amendment-id) ERR_AMENDMENT_NOT_FOUND))
      (lease (unwrap! (map-get? leases (get lease-id amendment)) ERR_LEASE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get proposer amendment)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status amendment) "pending") ERR_AMENDMENT_NOT_PENDING)
    
    (map-set lease-amendments amendment-id 
      (merge amendment { status: "withdrawn" })
    )
    (ok true)
  )
)

(define-read-only (get-amendment (amendment-id uint))
  (map-get? lease-amendments amendment-id)
)

(define-read-only (get-amendment-approval (amendment-id uint) (approver principal))
  (map-get? amendment-approval-history { amendment-id: amendment-id, approver: approver })
)

(define-read-only (get-amendment-counter)
  (var-get amendment-counter)
)

(define-read-only (get-lease-amendment-count (lease-id uint))
  (default-to u0 (map-get? lease-amendment-count lease-id))
)

(define-read-only (is-amendment-pending (amendment-id uint))
  (match (map-get? lease-amendments amendment-id)
    amendment (is-eq (get status amendment) "pending")
    false
  )
)

(define-read-only (is-amendment-approved (amendment-id uint))
  (match (map-get? lease-amendments amendment-id)
    amendment (and (get landlord-approved amendment) (get tenant-approved amendment))
    false
  )
)

(define-read-only (get-amendment-status (amendment-id uint))
  (match (map-get? lease-amendments amendment-id)
    amendment (get status amendment)
    "not-found"
  )
)

(define-read-only (is-amendment-expired (amendment-id uint))
  (match (map-get? lease-amendments amendment-id)
    amendment (>= stacks-block-height (get expiry-date amendment))
    false
  )
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes dispute-id)
)

(define-read-only (get-dispute-evidence (dispute-id uint) (evidence-id uint))
  (map-get? dispute-evidence { dispute-id: dispute-id, evidence-id: evidence-id })
)

(define-read-only (get-dispute-vote (dispute-id uint) (voter principal))
  (map-get? dispute-votes { dispute-id: dispute-id, voter: voter })
)

(define-read-only (get-dispute-counter)
  (var-get dispute-counter)
)

(define-read-only (get-arbitrator)
  (var-get arbitrator)
)

(define-read-only (get-evidence-count (dispute-id uint))
  (default-to u0 (map-get? dispute-evidence-counter dispute-id))
)



(define-read-only (is-dispute-resolved (dispute-id uint))
  (match (map-get? disputes dispute-id)
    dispute (is-eq (get status dispute) "resolved")
    false
  )
)

(define-read-only (get-dispute-status (dispute-id uint))
  (match (map-get? disputes dispute-id)
    dispute (get status dispute)
    "not-found"
  )
)


