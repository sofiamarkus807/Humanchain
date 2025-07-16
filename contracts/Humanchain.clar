(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-NOT-REGISTERED (err u102))
(define-constant ERR-INVALID-VOUCHER (err u103))
(define-constant ERR-ALREADY-VOUCHED (err u104))
(define-constant ERR-SELF-VOUCH (err u105))
(define-constant ERR-INSUFFICIENT-VOUCHES (err u106))
(define-constant ERR-COOLING-PERIOD (err u107))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u108))
(define-constant ERR-BADGE-ALREADY-EARNED (err u109))
(define-constant ERR-INVALID-BADGE-TYPE (err u110))
(define-constant ERR-NO-REWARDS-AVAILABLE (err u111))

(define-constant REQUIRED_VOUCHES u3)
(define-constant COOLING_PERIOD u144)
(define-constant REGISTRATION_COST u100)

(define-constant VOUCH_POINTS u10)
(define-constant REGISTRATION_POINTS u5)
(define-constant DAILY_DECAY_RATE u2)
(define-constant REPUTATION_DECAY_PERIOD u144)
(define-constant MIN_REPUTATION_FOR_REWARDS u50)
(define-constant BADGE-NEWCOMER u1)
(define-constant BADGE-CONTRIBUTOR u2)
(define-constant BADGE-GUARDIAN u3)
(define-constant BADGE-LEGEND u4)
(define-constant TIER-BRONZE u1)
(define-constant TIER-SILVER u2)
(define-constant TIER-GOLD u3)
(define-constant TIER-PLATINUM u4)

(define-data-var contract-owner principal tx-sender)
(define-data-var total-humans uint u0)
(define-data-var total-reputation-pool uint u0)
(define-data-var rewards-distributed uint u0)

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

(define-map reputation-scores principal
  {
    total-points: uint,
    last-activity: uint,
    tier: uint,
    badges-earned: (list 10 uint),
    total-vouches-given: uint,
    total-vouches-received: uint,
    decay-exempt-until: uint
  }
)

(define-map badge-achievements 
  { human: principal, badge-type: uint }
  { earned: bool, earned-at: uint }
)

(define-map daily-rewards principal
  { 
    last-claim: uint,
    total-claimed: uint,
    consecutive-days: uint
  }
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
    (unwrap-panic (award-reputation-points sender REGISTRATION_POINTS))
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
          (unwrap-panic (award-reputation-points sender VOUCH_POINTS))
           (unwrap-panic (award-reputation-points human (/ VOUCH_POINTS u2)))
           (unwrap-panic (update-vouch-counts sender human))
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

(define-private (award-reputation-points (human principal) (points uint))
  (let 
    (
      (current-rep (default-to 
        { 
          total-points: u0, 
          last-activity: u0, 
          tier: u0, 
          badges-earned: (list), 
          total-vouches-given: u0, 
          total-vouches-received: u0,
          decay-exempt-until: u0 
        } 
        (map-get? reputation-scores human)
      ))
      (decayed-points (calculate-decayed-points human))
      (new-total (+ decayed-points points))
      (new-tier (calculate-tier new-total))
    )
    (map-set reputation-scores human
      (merge current-rep
        {
          total-points: new-total,
          last-activity: stacks-block-height,
          tier: new-tier
        }
      )
    )
    (var-set total-reputation-pool (+ (var-get total-reputation-pool) points))
    (unwrap-panic (check-and-award-badges human new-total))
    (ok true)
  )
)

(define-private (calculate-decayed-points (human principal))
  (match (map-get? reputation-scores human)
    rep-data
      (let 
        (
          (blocks-since-activity (- stacks-block-height (get last-activity rep-data)))
          (decay-periods (/ blocks-since-activity REPUTATION_DECAY_PERIOD))
          (total-decay (* decay-periods DAILY_DECAY_RATE))
          (current-points (get total-points rep-data))
        )
        (if (> stacks-block-height (get decay-exempt-until rep-data))
          (if (> total-decay current-points)
            u0
            (- current-points total-decay)
          )
          current-points
        )
      )
    u0
  )
)

(define-private (calculate-tier (points uint))
  (if (>= points u1000) TIER-PLATINUM
    (if (>= points u500) TIER-GOLD
      (if (>= points u200) TIER-SILVER
        (if (>= points u50) TIER-BRONZE
          u0
        )
      )
    )
  )
)

(define-private (update-vouch-counts (voucher principal) (vouched principal))
  (let 
    (
      (voucher-rep (default-to 
        { 
          total-points: u0, 
          last-activity: u0, 
          tier: u0, 
          badges-earned: (list), 
          total-vouches-given: u0, 
          total-vouches-received: u0,
          decay-exempt-until: u0 
        } 
        (map-get? reputation-scores voucher)
      ))
      (vouched-rep (default-to 
        { 
          total-points: u0, 
          last-activity: u0, 
          tier: u0, 
          badges-earned: (list), 
          total-vouches-given: u0, 
          total-vouches-received: u0,
          decay-exempt-until: u0 
        } 
        (map-get? reputation-scores vouched)
      ))
    )
    (map-set reputation-scores voucher
      (merge voucher-rep
        { total-vouches-given: (+ (get total-vouches-given voucher-rep) u1) }
      )
    )
    (map-set reputation-scores vouched
      (merge vouched-rep
        { total-vouches-received: (+ (get total-vouches-received vouched-rep) u1) }
      )
    )
    (ok true)
  )
)

(define-private (check-and-award-badges (human principal) (total-points uint))
  (begin
    (if (and (>= total-points u10) (not (has-badge human BADGE-NEWCOMER)))
      (unwrap-panic (award-badge human BADGE-NEWCOMER))
      true
    )
    (if (and (>= total-points u100) (not (has-badge human BADGE-CONTRIBUTOR)))
      (unwrap-panic (award-badge human BADGE-CONTRIBUTOR))
      true
    )
    (if (and (>= total-points u500) (not (has-badge human BADGE-GUARDIAN)))
      (unwrap-panic (award-badge human BADGE-GUARDIAN))
      true
    )
    (if (and (>= total-points u1000) (not (has-badge human BADGE-LEGEND)))
      (unwrap-panic (award-badge human BADGE-LEGEND))
      true
    )
    (ok true)
  )
)

(define-private (award-badge (human principal) (badge-type uint))
  (let 
    (
      (badge-key { human: human, badge-type: badge-type })
      (current-rep (unwrap-panic (map-get? reputation-scores human)))
      (current-badges (get badges-earned current-rep))
    )
    (asserts! (not (has-badge human badge-type)) ERR-BADGE-ALREADY-EARNED)
    (map-set badge-achievements badge-key
      { earned: true, earned-at: stacks-block-height }
    )
    (map-set reputation-scores human
      (merge current-rep
        { badges-earned: (unwrap-panic (as-max-len? (append current-badges badge-type) u10)) }
      )
    )
    (ok true)
  )
)

(define-public (claim-daily-reward)
  (let 
    (
      (sender tx-sender)
      (current-rep (calculate-decayed-points sender))
      (reward-data (default-to 
        { last-claim: u0, total-claimed: u0, consecutive-days: u0 } 
        (map-get? daily-rewards sender)
      ))
      (blocks-since-claim (- stacks-block-height (get last-claim reward-data)))
      (is-consecutive (and (> blocks-since-claim u144) (< blocks-since-claim u288)))
      (reward-amount (calculate-daily-reward current-rep (get consecutive-days reward-data)))
    )
    (asserts! (is-registered sender) ERR-NOT-REGISTERED)
    (asserts! (>= current-rep MIN_REPUTATION_FOR_REWARDS) ERR-INSUFFICIENT-REPUTATION)
    (asserts! (> blocks-since-claim u144) ERR-NO-REWARDS-AVAILABLE)
    
    (map-set daily-rewards sender
      {
        last-claim: stacks-block-height,
        total-claimed: (+ (get total-claimed reward-data) reward-amount),
        consecutive-days: (if is-consecutive (+ (get consecutive-days reward-data) u1) u1)
      }
    )
    (var-set rewards-distributed (+ (var-get rewards-distributed) reward-amount))
    (try! (stx-transfer? reward-amount (as-contract tx-sender) sender))
    (ok reward-amount)
  )
)

(define-private (calculate-daily-reward (reputation uint) (consecutive-days uint))
  (let 
    (
      (base-reward u10)
      (tier-multiplier (if (>= reputation u1000) u4
                       (if (>= reputation u500) u3
                       (if (>= reputation u200) u2
                       u1))))
      (streak-bonus (if (> consecutive-days u10) u10 consecutive-days))
    )
    (* base-reward (+ tier-multiplier streak-bonus))
  )
)

(define-read-only (get-reputation (human principal))
  (let 
    (
      (current-points (calculate-decayed-points human))
    )
    (some {
      total-points: current-points,
      tier: (calculate-tier current-points),
      badges: (default-to (list) (get badges-earned (map-get? reputation-scores human)))
    })
  )
)

(define-read-only (has-badge (human principal) (badge-type uint))
  (default-to 
    false
    (get earned 
      (map-get? badge-achievements { human: human, badge-type: badge-type })
    )
  )
)

(define-read-only (get-daily-reward-info (human principal))
  (map-get? daily-rewards human)
)

(define-read-only (get-reputation-leaderboard)
  (ok {
    total-pool: (var-get total-reputation-pool),
    rewards-distributed: (var-get rewards-distributed)
  })
)