(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-NOT-REGISTERED (err u102))
(define-constant ERR-INVALID-VOUCHER (err u103))
(define-constant ERR-ALREADY-VOUCHED (err u104))
(define-constant ERR-SELF-VOUCH (err u105))
(define-constant ERR-INSUFFICIENT-VOUCHES (err u106))
(define-constant ERR-COOLING-PERIOD (err u107))

(define-constant REQUIRED_VOUCHES u3)
(define-constant COOLING_PERIOD u144)
(define-constant REGISTRATION_COST u100)

(define-data-var contract-owner principal tx-sender)
(define-data-var total-humans uint u0)

(define-map registered-humans principal 
  {
    registered: bool,
    registration-time: uint,
    vouch-count: uint,
    cooling-period-end: uint
  }
)

(define-map vouching-record 
  { voucher: principal, human: principal } 
  { vouched: bool }
)

(define-public (register)
  (let 
    (
      (sender tx-sender)
      (current-block-height stacks-block-height)
    )
    (asserts! (not (is-registered sender)) ERR-ALREADY-REGISTERED)
    (try! (stx-transfer? REGISTRATION_COST sender (as-contract tx-sender)))
    (map-set registered-humans sender
      {
        registered: true,
        registration-time: current-block-height,
        vouch-count: u0,
        cooling-period-end: u0
      }
    )
    (var-set total-humans (+ (var-get total-humans) u1))
    (ok true)
  )
)

(define-public (vouch-for (human principal))
  (let 
    (
      (sender tx-sender)
      (vouching-key { voucher: sender, human: human })
    )
    (asserts! (not (is-eq sender human)) ERR-SELF-VOUCH)
    (asserts! (is-registered sender) ERR-NOT-REGISTERED)
    (asserts! (is-registered human) ERR-NOT-REGISTERED)
    (asserts! (not (default-to false (get vouched (map-get? vouching-record vouching-key)))) ERR-ALREADY-VOUCHED)
    
    (map-set vouching-record vouching-key { vouched: true })
    (match (map-get? registered-humans human)
      registration-data
        (begin
          (map-set registered-humans 
            human 
            (merge registration-data 
              { vouch-count: (+ (get vouch-count registration-data) u1) }
            )
          )
          (ok true)
        )
      ERR-NOT-REGISTERED
    )
  )
)
(define-public (revoke-registration)
  (let 
    (
      (sender tx-sender)
      (current-block-height stacks-block-height)
    )
    (asserts! (is-registered sender) ERR-NOT-REGISTERED)
    (match (map-get? registered-humans sender)
      registration-data
        (begin
          (map-set registered-humans 
            sender 
            (merge registration-data 
              {
                registered: false,
                cooling-period-end: (+ stacks-block-height COOLING_PERIOD)
              }
            )
          )
          (var-set total-humans (- (var-get total-humans) u1))
          (ok true)
        )
      ERR-NOT-REGISTERED
    )
  )
)

(define-read-only (is-registered (human principal))
  (default-to 
    false 
    (get registered 
      (map-get? registered-humans human)
    )
  )
)

(define-read-only (get-human-details (human principal))
  (map-get? registered-humans human)
)

(define-read-only (get-total-humans)
  (var-get total-humans)
)

(define-read-only (can-register (human principal))
  (match (map-get? registered-humans human)
    registration-data
      (if (get registered registration-data)
        false
        (>= stacks-block-height (get cooling-period-end registration-data))
      )
    true
  )
)

(define-read-only (has-vouched (voucher principal) (human principal))
  (default-to 
    false
    (get vouched 
      (map-get? vouching-record { voucher: voucher, human: human })
    )
  )
)