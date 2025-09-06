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
(define-constant ERR-DISPUTE-EXISTS (err u112))
(define-constant ERR-DISPUTE-NOT-FOUND (err u113))
(define-constant ERR-DISPUTE-EXPIRED (err u114))
(define-constant ERR-DISPUTE-RESOLVED (err u115))
(define-constant ERR-ALREADY-VOTED (err u116))
(define-constant ERR-INSUFFICIENT-STAKE (err u117))
(define-constant ERR-SELF-DISPUTE (err u118))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u119))

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

(define-constant DISPUTE_STAKE u200)
(define-constant DISPUTE_DURATION u1008)
(define-constant MIN_VOTING_POWER u50)
(define-constant DISPUTE-STATUS-PENDING u1)
(define-constant DISPUTE-STATUS-RESOLVED-GUILTY u2)
(define-constant DISPUTE-STATUS-RESOLVED-INNOCENT u3)
(define-constant DISPUTE-STATUS-EXPIRED u4)

(define-data-var contract-owner principal tx-sender)
(define-data-var total-humans uint u0)
(define-data-var total-reputation-pool uint u0)
(define-data-var rewards-distributed uint u0)
(define-data-var total-disputes uint u0)
(define-data-var active-disputes uint u0)
(define-data-var total-activity-checkins uint u0)

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

(define-map disputes uint
  {
    accused: principal,
    accuser: principal,
    created-at: uint,
    expires-at: uint,
    status: uint,
    stake-amount: uint,
    guilty-votes: uint,
    innocent-votes: uint,
    total-voters: uint,
    evidence-hash: (buff 32)
  }
)

(define-map dispute-votes
  { dispute-id: uint, voter: principal }
  { 
    vote: bool,
    voting-power: uint,
    voted-at: uint
  }
)

(define-map dispute-history principal
  {
    total-accusations: uint,
    total-disputes-won: uint,
    total-disputes-lost: uint,
    reputation-penalty: uint,
    is-suspended: bool,
    suspension-end: uint
  }
)

(define-map human-activity-tracking principal
  {
    last-activity-checkin: uint,
    total-activity-checkins: uint,
    consecutive-activity-checkins: uint,
    longest-activity-streak: uint,
    activity-score: uint
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

(define-public (create-dispute (accused principal) (evidence-hash (buff 32)))
  (let 
    (
      (sender tx-sender)
      (dispute-id (+ (var-get total-disputes) u1))
      (current-rep (calculate-decayed-points sender))
      (accused-history (default-to 
        { 
          total-accusations: u0, 
          total-disputes-won: u0, 
          total-disputes-lost: u0, 
          reputation-penalty: u0,
          is-suspended: false,
          suspension-end: u0 
        } 
        (map-get? dispute-history accused)
      ))
    )
    (asserts! (not (is-eq sender accused)) ERR-SELF-DISPUTE)
    (asserts! (is-registered sender) ERR-NOT-REGISTERED)
    (asserts! (is-registered accused) ERR-NOT-REGISTERED)
    (asserts! (>= current-rep MIN_VOTING_POWER) ERR-INSUFFICIENT-VOTING-POWER)
    (asserts! (not (is-suspended accused)) ERR-DISPUTE-EXISTS)
    
    (try! (stx-transfer? DISPUTE_STAKE sender (as-contract tx-sender)))
    
    (map-set disputes dispute-id
      {
        accused: accused,
        accuser: sender,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height DISPUTE_DURATION),
        status: DISPUTE-STATUS-PENDING,
        stake-amount: DISPUTE_STAKE,
        guilty-votes: u0,
        innocent-votes: u0,
        total-voters: u0,
        evidence-hash: evidence-hash
      }
    )
    
    (map-set dispute-history accused
      (merge accused-history
        { total-accusations: (+ (get total-accusations accused-history) u1) }
      )
    )
    
    (var-set total-disputes dispute-id)
    (var-set active-disputes (+ (var-get active-disputes) u1))
    (ok dispute-id)
  )
)

(define-public (vote-on-dispute (dispute-id uint) (vote-guilty bool))
  (let 
    (
      (sender tx-sender)
      (dispute-data (unwrap! (map-get? disputes dispute-id) ERR-DISPUTE-NOT-FOUND))
      (voter-rep (calculate-decayed-points sender))
      (voting-key { dispute-id: dispute-id, voter: sender })
      (current-votes (get total-voters dispute-data))
      (current-guilty (get guilty-votes dispute-data))
      (current-innocent (get innocent-votes dispute-data))
    )
    (asserts! (is-registered sender) ERR-NOT-REGISTERED)
    (asserts! (>= voter-rep MIN_VOTING_POWER) ERR-INSUFFICIENT-VOTING-POWER)
    (asserts! (is-eq (get status dispute-data) DISPUTE-STATUS-PENDING) ERR-DISPUTE-RESOLVED)
    (asserts! (< stacks-block-height (get expires-at dispute-data)) ERR-DISPUTE-EXPIRED)
    (asserts! (is-none (map-get? dispute-votes voting-key)) ERR-ALREADY-VOTED)
    
    (map-set dispute-votes voting-key
      {
        vote: vote-guilty,
        voting-power: voter-rep,
        voted-at: stacks-block-height
      }
    )
    
    (map-set disputes dispute-id
      (merge dispute-data
        {
          guilty-votes: (if vote-guilty (+ current-guilty voter-rep) current-guilty),
          innocent-votes: (if vote-guilty current-innocent (+ current-innocent voter-rep)),
          total-voters: (+ current-votes u1)
        }
      )
    )
    
    (ok true)
  )
)

(define-public (resolve-dispute (dispute-id uint))
  (let 
    (
      (dispute-data (unwrap! (map-get? disputes dispute-id) ERR-DISPUTE-NOT-FOUND))
      (accused (get accused dispute-data))
      (accuser (get accuser dispute-data))
      (guilty-votes (get guilty-votes dispute-data))
      (innocent-votes (get innocent-votes dispute-data))
      (total-votes (+ guilty-votes innocent-votes))
      (is-guilty (> guilty-votes innocent-votes))
      (stake-amount (get stake-amount dispute-data))
      (accused-history (default-to 
        { 
          total-accusations: u0, 
          total-disputes-won: u0, 
          total-disputes-lost: u0, 
          reputation-penalty: u0,
          is-suspended: false,
          suspension-end: u0 
        } 
        (map-get? dispute-history accused)
      ))
      (new-status (if (>= stacks-block-height (get expires-at dispute-data))
                    DISPUTE-STATUS-EXPIRED
                    (if is-guilty DISPUTE-STATUS-RESOLVED-GUILTY DISPUTE-STATUS-RESOLVED-INNOCENT)))
    )
    (asserts! (is-eq (get status dispute-data) DISPUTE-STATUS-PENDING) ERR-DISPUTE-RESOLVED)
    (asserts! (or 
      (>= stacks-block-height (get expires-at dispute-data))
      (>= total-votes u10)
    ) ERR-DISPUTE-NOT-FOUND)
    
    (map-set disputes dispute-id
      (merge dispute-data { status: new-status })
    )
    
    (if (is-eq new-status DISPUTE-STATUS-RESOLVED-GUILTY)
      (begin
        (map-set dispute-history accused
          (merge accused-history
            {
              total-disputes-lost: (+ (get total-disputes-lost accused-history) u1),
              reputation-penalty: (+ (get reputation-penalty accused-history) u100),
              is-suspended: true,
              suspension-end: (+ stacks-block-height COOLING_PERIOD)
            }
          )
        )
        (try! (stx-transfer? (/ stake-amount u2) (as-contract tx-sender) accuser))
        (unwrap-panic (distribute-voter-rewards dispute-id stake-amount true))
      )
      (begin
        (map-set dispute-history accused
          (merge accused-history
            { total-disputes-won: (+ (get total-disputes-won accused-history) u1) }
          )
        )
        (try! (stx-transfer? stake-amount (as-contract tx-sender) accuser))
        (unwrap-panic (distribute-voter-rewards dispute-id u0 false))
      )
    )
    
    (var-set active-disputes (- (var-get active-disputes) u1))
    (ok new-status)
  )
)

(define-private (distribute-voter-rewards (dispute-id uint) (reward-pool uint) (guilty-won bool))
  (begin
    (ok true)
  )
)



(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes dispute-id)
)

(define-read-only (get-dispute-vote (dispute-id uint) (voter principal))
  (map-get? dispute-votes { dispute-id: dispute-id, voter: voter })
)

(define-read-only (get-dispute-history (human principal))
  (map-get? dispute-history human)
)

(define-read-only (is-suspended (human principal))
  (match (map-get? dispute-history human)
    history
      (and 
        (get is-suspended history)
        (> (get suspension-end history) stacks-block-height)
      )
    false
  )
)

(define-read-only (get-dispute-stats)
  (ok {
    total-disputes: (var-get total-disputes),
    active-disputes: (var-get active-disputes)
  })
)

;; Human Activity Verification System
(define-public (activity-checkin)
  (let 
    (
      (sender tx-sender)
      (current-block stacks-block-height)
      (activity-data (default-to 
        {
          last-activity-checkin: u0,
          total-activity-checkins: u0,
          consecutive-activity-checkins: u0,
          longest-activity-streak: u0,
          activity-score: u0
        }
        (map-get? human-activity-tracking sender)
      ))
      (blocks-since-last (if (is-eq (get last-activity-checkin activity-data) u0) u0 (- current-block (get last-activity-checkin activity-data))))
      (is-consecutive (or (is-eq (get last-activity-checkin activity-data) u0) (<= blocks-since-last u1008)))
      (new-consecutive (if is-consecutive (+ (get consecutive-activity-checkins activity-data) u1) u1))
      (new-longest (if (> new-consecutive (get longest-activity-streak activity-data)) new-consecutive (get longest-activity-streak activity-data)))
    )
    (asserts! (is-registered sender) ERR-NOT-REGISTERED)
    
    ;; Update activity tracking data
    (map-set human-activity-tracking sender
      (merge activity-data
        {
          last-activity-checkin: current-block,
          total-activity-checkins: (+ (get total-activity-checkins activity-data) u1),
          consecutive-activity-checkins: new-consecutive,
          longest-activity-streak: new-longest,
          activity-score: (+ (get activity-score activity-data) (calculate-activity-points new-consecutive))
        }
      )
    )
    
    ;; Update global counter
    (var-set total-activity-checkins (+ (var-get total-activity-checkins) u1))
    
    ;; Award reputation points for consistent activity
    (unwrap-panic (award-reputation-points sender (calculate-activity-points new-consecutive)))
    (ok new-consecutive)
  )
)

(define-private (calculate-activity-points (consecutive uint))
  (if (> consecutive u30) u25
    (if (> consecutive u14) u20
      (if (> consecutive u7) u15
        (if (> consecutive u3) u10
          u5
        )
      )
    )
  )
)

(define-read-only (get-human-activity (human principal))
  (map-get? human-activity-tracking human)
)

(define-read-only (is-human-recently-active (human principal))
  (match (map-get? human-activity-tracking human)
    activity-data
    (let 
      (
        (blocks-since-checkin (- stacks-block-height (get last-activity-checkin activity-data)))
      )
      (<= blocks-since-checkin u1008)
    )
    false
  )
)

(define-read-only (get-activity-stats)
  (ok {
    total-activity-checkins: (var-get total-activity-checkins)
  })
)

