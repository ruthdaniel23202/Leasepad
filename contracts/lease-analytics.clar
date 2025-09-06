;; Lease Performance Analytics Contract
;; Tracks lease performance metrics, payment patterns, and portfolio analytics

(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_LEASE_NOT_FOUND (err u201))
(define-constant ERR_ANALYTICS_NOT_FOUND (err u202))
(define-constant ERR_INVALID_TIMEFRAME (err u203))

;; Track payment performance for each lease
(define-map lease-payment-stats
  uint ;; lease-id
  {
    total-payments: uint,
    late-payments: uint,
    on-time-payments: uint,
    total-amount-paid: uint,
    avg-days-late: uint,
    payment-reliability-score: uint, ;; 0-100 scale
    last-payment-block: uint
  }
)

;; Track landlord portfolio performance
(define-map landlord-portfolio-stats
  principal ;; landlord address
  {
    total-leases: uint,
    active-leases: uint,
    terminated-leases: uint,
    total-rental-income: uint,
    avg-monthly-income: uint,
    occupancy-rate: uint, ;; percentage
    tenant-satisfaction-score: uint, ;; based on disputes/terminations
    portfolio-value-estimate: uint
  }
)

;; Track tenant performance across all leases
(define-map tenant-performance
  principal ;; tenant address
  {
    total-leases: uint,
    completed-leases: uint,
    early-terminations: uint,
    total-disputes: uint,
    average-payment-delay: uint,
    credit-score: uint, ;; 0-100 based on payment history
    reliability-rating: (string-ascii 10) ;; "excellent", "good", "fair", "poor"
  }
)

;; Monthly analytics snapshots for trend analysis
(define-map monthly-analytics
  { landlord: principal, month: uint }
  {
    rental-income: uint,
    vacancy-rate: uint,
    late-payment-rate: uint,
    new-leases: uint,
    terminated-leases: uint,
    dispute-count: uint,
    maintenance-costs: uint
  }
)

;; Market comparison data
(define-map market-insights
  (string-ascii 50) ;; property area/type
  {
    avg-rent-price: uint,
    avg-lease-duration: uint,
    vacancy-rate: uint,
    dispute-rate: uint,
    market-trend: (string-ascii 10), ;; "rising", "stable", "falling"
    last-updated: uint
  }
)

;; Initialize analytics for a lease
(define-public (initialize-lease-analytics (lease-id uint))
  (begin
    (asserts! (is-authorized-for-lease lease-id) ERR_NOT_AUTHORIZED)
    (map-set lease-payment-stats lease-id
      {
        total-payments: u0,
        late-payments: u0,
        on-time-payments: u0,
        total-amount-paid: u0,
        avg-days-late: u0,
        payment-reliability-score: u100,
        last-payment-block: u0
      }
    )
    (ok true)
  )
)

;; Record a payment and update analytics
(define-public (record-payment-analytics 
  (lease-id uint) 
  (amount uint) 
  (is-late bool) 
  (days-late uint))
  (let
    (
      (current-stats (default-to 
        {
          total-payments: u0,
          late-payments: u0,
          on-time-payments: u0,
          total-amount-paid: u0,
          avg-days-late: u0,
          payment-reliability-score: u100,
          last-payment-block: u0
        }
        (map-get? lease-payment-stats lease-id)
      ))
    )
    (asserts! (is-authorized-for-lease lease-id) ERR_NOT_AUTHORIZED)
    
    (let
      (
        (new-total-payments (+ (get total-payments current-stats) u1))
        (new-late-payments (if is-late 
          (+ (get late-payments current-stats) u1) 
          (get late-payments current-stats)))
        (new-on-time-payments (if (not is-late) 
          (+ (get on-time-payments current-stats) u1) 
          (get on-time-payments current-stats)))
        (new-total-amount (+ (get total-amount-paid current-stats) amount))
        (new-avg-days-late (if is-late
          (/ (+ (* (get avg-days-late current-stats) (get total-payments current-stats)) days-late)
             new-total-payments)
          (get avg-days-late current-stats)))
        (reliability-score (/ (* new-on-time-payments u100) new-total-payments))
      )
      
      (map-set lease-payment-stats lease-id
        {
          total-payments: new-total-payments,
          late-payments: new-late-payments,
          on-time-payments: new-on-time-payments,
          total-amount-paid: new-total-amount,
          avg-days-late: new-avg-days-late,
          payment-reliability-score: reliability-score,
          last-payment-block: stacks-block-height
        }
      )
      (ok true)
    )
  )
)

;; Update landlord portfolio statistics
(define-public (update-landlord-portfolio (landlord principal))
  (let
    (
      (current-stats (default-to
        {
          total-leases: u0,
          active-leases: u0,
          terminated-leases: u0,
          total-rental-income: u0,
          avg-monthly-income: u0,
          occupancy-rate: u0,
          tenant-satisfaction-score: u100,
          portfolio-value-estimate: u0
        }
        (map-get? landlord-portfolio-stats landlord)
      ))
    )
    (asserts! (is-eq tx-sender landlord) ERR_NOT_AUTHORIZED)
    
    ;; Update portfolio stats (simplified calculation)
    (map-set landlord-portfolio-stats landlord
      (merge current-stats
        {
          total-leases: (+ (get total-leases current-stats) u1)
        }
      )
    )
    (ok true)
  )
)

;; Generate performance report for a lease
(define-public (generate-lease-report (lease-id uint))
  (let
    (
      (stats (unwrap! (map-get? lease-payment-stats lease-id) ERR_ANALYTICS_NOT_FOUND))
    )
    (asserts! (is-authorized-for-lease lease-id) ERR_NOT_AUTHORIZED)
    
    (ok {
      lease-id: lease-id,
      payment-reliability: (get payment-reliability-score stats),
      total-income: (get total-amount-paid stats),
      payment-punctuality: (/ (* (get on-time-payments stats) u100) (get total-payments stats)),
      risk-assessment: (if (< (get payment-reliability-score stats) u60) "high" 
                        (if (< (get payment-reliability-score stats) u80) "medium" "low")),
      recommendation: (if (> (get payment-reliability-score stats) u90) 
                       "excellent-tenant" 
                       (if (> (get payment-reliability-score stats) u70) 
                         "good-tenant" 
                         "monitor-closely"))
    })
  )
)

;; Calculate portfolio ROI
(define-public (calculate-portfolio-roi (landlord principal) (initial-investment uint))
  (let
    (
      (portfolio-stats (unwrap! (map-get? landlord-portfolio-stats landlord) ERR_ANALYTICS_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender landlord) ERR_NOT_AUTHORIZED)
    (asserts! (> initial-investment u0) ERR_INVALID_TIMEFRAME)
    
    (let
      (
        (roi-percentage (/ (* (get total-rental-income portfolio-stats) u100) initial-investment))
        (monthly-yield (/ (get avg-monthly-income portfolio-stats) initial-investment))
      )
      (ok {
        total-roi: roi-percentage,
        monthly-yield: monthly-yield,
        total-income: (get total-rental-income portfolio-stats),
        active-properties: (get active-leases portfolio-stats),
        occupancy-rate: (get occupancy-rate portfolio-stats),
        performance-grade: (if (> roi-percentage u15) "A"
                            (if (> roi-percentage u10) "B"
                              (if (> roi-percentage u5) "C" "D")))
      })
    )
  )
)

;; Record market insights
(define-public (record-market-data 
  (area (string-ascii 50))
  (avg-rent uint)
  (avg-duration uint)
  (vacancy-rate uint)
  (trend (string-ascii 10)))
  (begin
    (map-set market-insights area
      {
        avg-rent-price: avg-rent,
        avg-lease-duration: avg-duration,
        vacancy-rate: vacancy-rate,
        dispute-rate: u5, ;; default 5%
        market-trend: trend,
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Private helper functions
(define-private (is-authorized-for-lease (lease-id uint))
  true ;; Simplified - would integrate with main lease contract
)

;; Read-only functions
(define-read-only (get-lease-analytics (lease-id uint))
  (map-get? lease-payment-stats lease-id)
)

(define-read-only (get-landlord-portfolio (landlord principal))
  (map-get? landlord-portfolio-stats landlord)
)

(define-read-only (get-tenant-performance (tenant principal))
  (map-get? tenant-performance tenant)
)

(define-read-only (get-market-insights (area (string-ascii 50)))
  (map-get? market-insights area)
)

(define-read-only (get-monthly-analytics (landlord principal) (month uint))
  (map-get? monthly-analytics { landlord: landlord, month: month })
)
