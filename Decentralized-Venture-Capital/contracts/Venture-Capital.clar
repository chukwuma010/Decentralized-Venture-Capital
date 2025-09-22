;; Decentralized Venture Capital Contract
;; Community-driven startup funding with transparent voting

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-EXISTS (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-VOTING-ENDED (err u104))
(define-constant ERR-VOTING-ACTIVE (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-INSUFFICIENT-STAKE (err u107))
(define-constant ERR-PROPOSAL-EXECUTED (err u108))
(define-constant ERR-QUORUM-NOT-MET (err u109))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-PROPOSAL-STAKE u1000000) ;; 1 STX in microSTX
(define-constant VOTING-PERIOD u1008) ;; ~1 week in blocks
(define-constant QUORUM-THRESHOLD u50) ;; 50% participation required

;; Data variables
(define-data-var proposal-counter uint u0)
(define-data-var total-fund-balance uint u0)

;; Data maps
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    startup-name: (string-ascii 64),
    description: (string-ascii 256),
    funding-amount: uint,
    votes-for: uint,
    votes-against: uint,
    total-voters: uint,
    voting-end-block: uint,
    executed: bool,
    approved: bool
  }
)

(define-map member-stakes
  { member: principal }
  { stake: uint, voting-power: uint }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voting-power: uint }
)

(define-map startup-funds
  { startup: principal }
  { allocated-amount: uint, released-amount: uint }
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-member-stake (member principal))
  (map-get? member-stakes { member: member })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-startup-funds (startup principal))
  (map-get? startup-funds { startup: startup })
)

(define-read-only (get-total-fund-balance)
  (var-get total-fund-balance)
)

(define-read-only (get-proposal-counter)
  (var-get proposal-counter)
)

(define-read-only (calculate-voting-power (stake uint))
  ;; Square root of stake for quadratic voting (simplified)
  (if (>= stake u1000000)
    (+ u1 (/ (- stake u1000000) u1000000))
    u1
  )
)

;; Public functions

;; Join as VC member by staking STX
(define-public (join-as-member (stake-amount uint))
  (let (
    (current-stake (default-to { stake: u0, voting-power: u0 } 
                               (map-get? member-stakes { member: tx-sender })))
    (new-stake (+ (get stake current-stake) stake-amount))
    (new-voting-power (calculate-voting-power new-stake))
  )
    (asserts! (> stake-amount u0) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (var-set total-fund-balance (+ (var-get total-fund-balance) stake-amount))
    (map-set member-stakes 
      { member: tx-sender }
      { stake: new-stake, voting-power: new-voting-power }
    )
    (ok new-voting-power)
  )
)

;; Submit funding proposal
(define-public (submit-proposal 
  (startup-name (string-ascii 64))
  (description (string-ascii 256))
  (funding-amount uint)
  (startup-address principal))
  (let (
    (proposal-id (+ (var-get proposal-counter) u1))
    (member-info (unwrap! (map-get? member-stakes { member: tx-sender }) ERR-NOT-AUTHORIZED))
  )
    (asserts! (>= (get stake member-info) MIN-PROPOSAL-STAKE) ERR-INSUFFICIENT-STAKE)
    (asserts! (> funding-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= funding-amount (var-get total-fund-balance)) ERR-INVALID-AMOUNT)
    
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        startup-name: startup-name,
        description: description,
        funding-amount: funding-amount,
        votes-for: u0,
        votes-against: u0,
        total-voters: u0,
        voting-end-block: (+ block-height VOTING-PERIOD),
        executed: false,
        approved: false
      }
    )
    
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

;; Vote on proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NOT-FOUND))
    (member-info (unwrap! (map-get? member-stakes { member: tx-sender }) ERR-NOT-AUTHORIZED))
    (voting-power (get voting-power member-info))
    (current-votes-for (get votes-for proposal))
    (current-votes-against (get votes-against proposal))
    (current-total-voters (get total-voters proposal))
  )
    (asserts! (< block-height (get voting-end-block proposal)) ERR-VOTING-ENDED)
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-for, voting-power: voting-power }
    )
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal {
        votes-for: (if vote-for (+ current-votes-for voting-power) current-votes-for),
        votes-against: (if vote-for current-votes-against (+ current-votes-against voting-power)),
        total-voters: (+ current-total-voters u1)
      })
    )
    
    (ok true)
  )
)

;; Execute proposal after voting period
(define-public (execute-proposal (proposal-id uint) (startup-address principal))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NOT-FOUND))
    (total-voting-power (get-total-voting-power))
    (participation-rate (/ (* (+ (get votes-for proposal) (get votes-against proposal)) u100) total-voting-power))
  )
    (asserts! (>= block-height (get voting-end-block proposal)) ERR-VOTING-ACTIVE)
    (asserts! (not (get executed proposal)) ERR-PROPOSAL-EXECUTED)
    (asserts! (>= participation-rate QUORUM-THRESHOLD) ERR-QUORUM-NOT-MET)
    
    (let (
      (approved (> (get votes-for proposal) (get votes-against proposal)))
    )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { executed: true, approved: approved })
      )
      
      (if approved
        (begin
          ;; Allocate funds to startup
          (map-set startup-funds
            { startup: startup-address }
            { 
              allocated-amount: (get funding-amount proposal),
              released-amount: u0
            }
          )
          (var-set total-fund-balance (- (var-get total-fund-balance) (get funding-amount proposal)))
          (ok { approved: true, amount: (get funding-amount proposal) })
        )
        (ok { approved: false, amount: u0 })
      )
    )
  )
)

;; Release funds to approved startup
(define-public (release-funds (startup-address principal) (amount uint))
  (let (
    (startup-fund (unwrap! (map-get? startup-funds { startup: startup-address }) ERR-NOT-FOUND))
    (available-amount (- (get allocated-amount startup-fund) (get released-amount startup-fund)))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= amount available-amount) ERR-INVALID-AMOUNT)
    
    (try! (as-contract (stx-transfer? amount tx-sender startup-address)))
    
    (map-set startup-funds
      { startup: startup-address }
      (merge startup-fund { released-amount: (+ (get released-amount startup-fund) amount) })
    )
    
    (ok amount)
  )
)

;; Withdraw stake (partial)
(define-public (withdraw-stake (amount uint))
  (let (
    (member-info (unwrap! (map-get? member-stakes { member: tx-sender }) ERR-NOT-AUTHORIZED))
    (current-stake (get stake member-info))
    (min-required-stake MIN-PROPOSAL-STAKE)
  )
    (asserts! (> current-stake min-required-stake) ERR-INSUFFICIENT-STAKE)
    (asserts! (<= amount (- current-stake min-required-stake)) ERR-INVALID-AMOUNT)
    
    (let (
      (new-stake (- current-stake amount))
      (new-voting-power (calculate-voting-power new-stake))
    )
      (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
      (var-set total-fund-balance (- (var-get total-fund-balance) amount))
      
      (map-set member-stakes
        { member: tx-sender }
        { stake: new-stake, voting-power: new-voting-power }
      )
      
      (ok amount)
    )
  )
)

;; Helper function to calculate total voting power
(define-private (get-total-voting-power)
  ;; Simplified calculation - in practice, you'd iterate through all members
  ;; This is a placeholder that should be implemented with a proper iteration mechanism
  u100 ;; Placeholder value
)

;; Emergency functions (only contract owner)
(define-public (emergency-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    ;; Emergency pause logic would go here
    (ok true)
  )
)

;; Get proposal status
(define-read-only (get-proposal-status (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal
    (let (
      (voting-ended (>= block-height (get voting-end-block proposal)))
      (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
    )
      (ok {
        exists: true,
        voting-ended: voting-ended,
        executed: (get executed proposal),
        approved: (get approved proposal),
        votes-for: (get votes-for proposal),
        votes-against: (get votes-against proposal),
        total-votes: total-votes
      })
    )
    (ok { exists: false, voting-ended: false, executed: false, approved: false, votes-for: u0, votes-against: u0, total-votes: u0 })
  )
)