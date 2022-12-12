;; holds Zest rewards to stakers
(impl-trait .distribution-token-cycles-losses-trait.distribution-token-cycles-losses-trait)
(impl-trait .ownable-trait.ownable-trait)

(use-trait ft .ft-trait.ft-trait)

(define-fungible-token cp-001)
(define-map token-balances {token-id: uint, owner: principal} uint)
(define-map token-supplies uint uint)

(define-data-var token-uri (string-utf8 256) u"")
(define-map token-uris uint (string-ascii 256))

(define-data-var contract-owner principal tx-sender)
(define-constant ERR_UNAUTHORIZED (err u1000))

;; -- sip-010 functions
(define-private (set-balance (token-id uint) (balance uint) (owner principal))
	(map-set token-balances {token-id: token-id, owner: owner} balance))

(define-read-only (get-balance-uint (token-id uint) (who principal))
	(default-to u0 (map-get? token-balances {token-id: token-id, owner: who})))

(define-read-only (get-balance (token-id uint) (who principal))
	(ok (get-balance-uint token-id who)))

(define-read-only (get-overall-balance (who principal))
	(ok (ft-get-balance cp-001 who)))

(define-read-only (get-total-supply (token-id uint))
	(ok (get-total-supply-uint token-id)))

(define-read-only (get-total-supply-uint (token-id uint))
	(default-to u0 (map-get? token-supplies token-id)))

(define-read-only (get-overall-supply)
	(ok (ft-get-supply cp-001)))

(define-read-only (get-decimals (token-id uint))
	(ok u0))

(define-public (set-token-uri (token-id uint) (value (string-ascii 256)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (ok (map-set token-uris token-id value))))

(define-read-only (get-token-uri (token-id uint))
	(ok (map-get? token-uris token-id)))

(define-public (transfer (token-id uint) (amount uint) (sender principal) (recipient principal))
  (let (
    (sender-balance (get-balance-uint token-id sender))
    (points-mag (to-int (* amount (get-points-per-share token-id))))
    (points-correction-from (+ (get-points-correction token-id sender) points-mag))
    (points-correction-to (+ (get-points-correction token-id recipient) points-mag))
    (loss-mag (to-int (* amount (get-losses-per-share token-id))))
    (loss-correction-from (+ (get-losses-correction token-id sender) points-mag))
    (loss-correction-to (+ (get-losses-correction token-id recipient) points-mag)))
    (map-set points-correction { token-id: token-id, owner: sender } points-correction-from)
    (map-set points-correction { token-id: token-id, owner: recipient } points-correction-to)
    (map-set losses-correction { token-id: token-id, owner: sender } loss-correction-from)
    (map-set losses-correction { token-id: token-id, owner: recipient } loss-correction-to)
		(asserts! (is-eq sender tx-sender) ERR_UNAUTHORIZED)
		(asserts! (<= amount sender-balance) ERR_INSUFFICIENT_BALANCE)
		(set-balance token-id (- sender-balance amount) sender)
		(set-balance token-id (+ (get-balance-uint token-id recipient) amount) recipient)
		(try! (ft-transfer? cp-001 amount sender recipient))
		(print {type: "sft_transfer", token-id: token-id, amount: amount, sender: sender, recipient: recipient})
		(ok true)))

(define-public (transfer-memo (token-id uint) (amount uint) (sender principal) (recipient principal) (memo (buff 34)))
	(begin
		(try! (transfer token-id amount sender recipient))
		(print memo)
		(ok true)))

(define-private (transfer-many-iter (item {token-id: uint, amount: uint, sender: principal, recipient: principal}) (previous-response (response bool uint)))
	(match previous-response prev-ok (transfer (get token-id item) (get amount item) (get sender item) (get recipient item)) prev-err previous-response))

(define-public (transfer-many (transfers (list 200 {token-id: uint, amount: uint, sender: principal, recipient: principal})))
	(fold transfer-many-iter transfers (ok true)))

(define-private (transfer-many-memo-iter (item {token-id: uint, amount: uint, sender: principal, recipient: principal, memo: (buff 34)}) (previous-response (response bool uint)))
	(match previous-response prev-ok (transfer-memo (get token-id item) (get amount item) (get sender item) (get recipient item) (get memo item)) prev-err previous-response))

(define-public (transfer-many-memo (transfers (list 200 {token-id: uint, amount: uint, sender: principal, recipient: principal, memo: (buff 34)})))
	(fold transfer-many-memo-iter transfers (ok true)))

;; -- distribution-token-trait --
;; Uses Funds Distribution Token model from : https://github.com/ethereum/EIPs/issues/2222
;; token circulation can be of 2^112 because:
;; Max satoshi :
;; 2.1 * 10^7 * 10^8  = 2.1 * 10^15
;; require 2^51 precision
;; and max uint precision is 2^128

;; -- Recognize earnings --

(define-constant POINTS_MULTIPLIER (pow u2 u64))

(define-map points-per-share uint uint)
(define-map points-correction { token-id: uint, owner: principal } int)
(define-map withdrawn-funds { token-id: uint, owner: principal } uint)

;; GETTERS
(define-read-only (get-points-per-share (token-id uint))
  (default-to u0 (map-get? points-per-share token-id)))

(define-read-only (get-points-correction (token-id uint) (owner principal))
  (default-to 0 (map-get? points-correction { token-id: token-id, owner: owner })))

(define-read-only (get-withdrawn-funds (token-id uint) (owner principal))
  (default-to u0 (map-get? withdrawn-funds { token-id: token-id, owner: owner })))

(define-read-only (get-sent-funds (token-id uint) (owner principal))
  (/ (get-balance-uint token-id owner) DFT_PRECISION))

(define-read-only (get-cycle-at (token-id uint) (stacks-height uint))
  (let (
    (first-block (get-cycle-start token-id))
    (pool (contract-call? .cover-pool-v1-0 get-pool-read token-id))
    (cycle-length (get cycle-length pool)))
    (if (>= stacks-height first-block)
      (some (/ (- stacks-height first-block) cycle-length))
      none)))

(define-read-only (get-current-cycle (token-id uint))
  (let (
    (pool (contract-call? .cover-pool-v1-0 get-pool-read token-id))
    (cycle-length (get cycle-length pool))
    (first-block (get-cycle-start token-id))
    (stacks-height block-height))
    (if (>= stacks-height first-block)
      (some (/ (- stacks-height first-block) cycle-length))
      none)))

(define-read-only (get-next-cycle-height (token-id uint))
  (let (
    (pool (contract-call? .cover-pool-v1-0 get-pool-read token-id))
    (cycle-length (get cycle-length pool))
    (first-block (get-cycle-start token-id))
    (stacks-height block-height))
    (if (>= stacks-height first-block)
      (some (+ first-block (* cycle-length (+ u1 (/ (- stacks-height first-block) cycle-length)))))
      none)))

(define-constant DFT_PRECISION (pow u10 u5))

;; @desc withdraw rewards from caller in selecter pool id
;; @restricted pool
;; @param token-id: pool id
;; @param caller: user withdrawing their rewards
;; @returns (response uint uint)
(define-public (withdraw-rewards (token-id uint) (caller principal))
  (let (
    (withdrawable-funds (withdrawable-funds-of token-id caller)))
    (try! (is-approved-contract contract-caller))
    (map-set withdrawn-funds { token-id: token-id , owner: caller } (+ withdrawable-funds (get-withdrawn-funds token-id caller)))

    (ok withdrawable-funds)))

;; @desc get all cycle rewards since start of commitment to current-cycle
;; @restricted pool
;; @param token-id: pool id
;; @param caller: user withdrawing their rewards
;; @returns (response { cycle-rewards: uint, passive-rewards: uint } uint)
(define-public (withdraw-cycle-rewards (token-id uint) (caller principal))
  (let (
    (funds-sent (unwrap-panic (contract-call? .cover-pool-data get-sent-funds caller token-id)))
    (start-cycle (unwrap-panic (get-cycle-at token-id (get start funds-sent))))
    (current-cycle (unwrap-panic (get-current-cycle token-id)))
    (last-commitment-cycle (+ (get cycles funds-sent) start-cycle))
    (end-cycle (if (>= (+ u1 last-commitment-cycle) current-cycle) current-cycle last-commitment-cycle))
    (sum (unwrap! (remove-share-cycle (+ u1 start-cycle) end-cycle token-id caller) ERR_NOT_ENOUGH_TIME_PASSED))
    (withdrawable-funds (unwrap-panic (withdraw-rewards token-id caller)))
    (passive-rewards u0))
    (try! (is-approved-contract contract-caller))
    (print { type: "withdraw-cycle-rewards-cp-token", payload: { token-id: token-id, caller: caller } })

    (ok { cycle-rewards: sum, passive-rewards: passive-rewards })))

;; @desc get possible cycle rewards when calling withdraw-cycle-rewards
;; @param token-id: pool id
;; @param recipient: user being queried
;; @returns (response { cycle-rewards: uint, passive-rewards: uint } uint)
(define-read-only (get-withdrawable-rewards (token-id uint) (recipient principal))
  (let (
    (funds-sent (contract-call? .cover-pool-data get-sent-funds-read recipient token-id))
    (current-cycle (unwrap-panic (get-current-cycle token-id)))
    (start-cycle (unwrap-panic (get-cycle-at token-id (get start funds-sent))))
    (last-commitment-cycle (+ (get cycles funds-sent) start-cycle))
    (end-cycle (if (>= (+ u1 last-commitment-cycle) current-cycle) current-cycle last-commitment-cycle))
    (sum (unwrap-panic (get-sum-cycles (+ u1 start-cycle) end-cycle token-id recipient)))
    (withdrawable-funds (withdrawable-funds-of-read token-id recipient))
    (passive-rewards u0))
    { cycle-rewards: sum, passive-rewards: passive-rewards }))

;; @desc called by pool to add rewards acquired at current block-height
;; @restricted pool
;; @param token-id: pool id
;; @param delta: rewards earned
;; @returns (response uint uint)
(define-public (add-rewards (token-id uint) (delta uint))
  (let (
    (total-supply (get-total-supply-uint token-id))
    (valid-token-id (asserts! (> total-supply u0) (ok u0)))
    (added-points (/ (* delta POINTS_MULTIPLIER) total-supply))
    (total-points-shared (+ (get-points-per-share token-id) added-points)))
    (try! (is-approved-contract contract-caller))
    (set-rewards token-id delta block-height)
    (map-set points-per-share token-id total-points-shared)
    (print { type: "added_funds", added-points: added-points })
    (ok total-points-shared)))

;; @desc add to the amount of rewards in cycle and chose pool
;; @restricted pool
;; @param token-id: pool id
;; @param amount: rewards being distributed
;; @param time: block height at which rewards were received
;; @returns uint
(define-private (set-rewards (token-id uint) (amount uint) (time uint))
  (let (
    (cycle (unwrap-panic (get-cycle-at token-id time)))
    (cycle-rewards (get-cycle-rewards token-id cycle)))
    (map-set rewards { token-id: token-id, cycle: cycle } (+ amount cycle-rewards))

    (print { type: "set-rewards-cp-token", payload: { token-id: token-id, cycle: cycle, rewards: (+ amount cycle-rewards) } })
    cycle))

;; @desc mint tokens that can claim rewards earned
;; @restricted pool
;; @param token-id: pool id
;; @param amount: amount to be minted
;; @param recipient: recipient of minted tokens
;; @returns (response true uint)
(define-public (mint (token-id uint) (amount uint) (recipient principal))
	(begin
    (try! (is-approved-contract contract-caller))
		(try! (ft-mint? cp-001 amount recipient))
		(set-balance token-id (+ (get-balance-uint token-id recipient) amount) recipient)
		(map-set token-supplies token-id (+ (unwrap-panic (get-total-supply token-id)) amount))
    (mint-priv token-id amount recipient)
		(print {type: "sft_mint", token-id: token-id, amount: amount, recipient: recipient})
		(ok true)))

;; @desc burn tokens that can claim rewards earned
;; @restricted pool
;; @param token-id: pool id
;; @param amount: amount to be minted
;; @param owner: owner of tokens to be burned
;; @returns (response true uint)
(define-public (burn (token-id uint) (amount uint) (owner principal))
 (begin
    (try! (is-approved-contract contract-caller))
    (try! (ft-burn? cp-001 amount owner))
    (set-balance token-id (- (get-balance-uint token-id owner) amount) owner)
		(map-set token-supplies token-id (- (unwrap-panic (get-total-supply token-id)) amount))
    (burn-priv token-id amount owner)
		(print {type: "sft_burn", token-id: token-id, amount: amount, owner: owner})
    (ok true)))

;; @desc correct for the amount being burned
;; @restricted pool
;; @param token-id: pool id
;; @param amount: amount to be minted
;; @param owner: owner of tokens to be burned
;; @returns true
(define-private (burn-priv (token-id uint) (amount uint) (owner principal))
  (let (
    (point-correction (to-int (* amount (get-points-per-share token-id))))
    (loss-correction (to-int (* amount (get-losses-per-share token-id)))))
    (map-set points-correction { owner: owner, token-id: token-id }
      (+ (get-points-correction token-id owner) point-correction))
    (map-set losses-correction { owner: owner, token-id: token-id }
      (+ (get-losses-correction token-id owner) loss-correction))))

;; @desc correct for the amount being minted
;; @restricted pool
;; @param token-id: pool id
;; @param amount: amount to be minted
;; @param owner: owner of tokens to be minted
;; @returns true
(define-private (mint-priv (token-id uint) (amount uint) (recipient principal))
  (let (
    (point-correction (to-int (* amount (get-points-per-share token-id))))
    (loss-correction (to-int (* amount (get-losses-per-share token-id)))))
    (map-set points-correction { owner: recipient, token-id: token-id }
      (- (get-points-correction token-id recipient) point-correction))
    (map-set losses-correction { owner: recipient, token-id: token-id }
      (- (get-losses-correction token-id recipient) loss-correction))))

(define-read-only (withdrawable-funds-of (token-id uint) (owner principal))
  (- (accumulative-funds-of token-id owner) (get-withdrawn-funds token-id owner)))

(define-read-only (withdrawable-funds-of-read (token-id uint) (owner principal))
  (- (accumulative-funds-of token-id owner) (get-withdrawn-funds token-id owner)))

(define-read-only (accumulative-funds-of (token-id uint) (owner principal))
  (begin
    (/
      (to-uint (+
        (to-int (* (get-points-per-share token-id) (get-balance-uint token-id owner)))
        (get-points-correction token-id owner)
      ))
    POINTS_MULTIPLIER)))

;; -- Recognize losses --
(define-map losses-per-share uint uint)
(define-map losses-correction { token-id: uint, owner: principal } int)
(define-map recognized-losses { token-id: uint, owner: principal } uint)

(define-read-only (get-losses-per-share (token-id uint))
  (default-to u0 (map-get? losses-per-share token-id)))

(define-read-only (get-losses-correction (token-id uint) (owner principal))
  (default-to 0 (map-get? losses-correction { token-id: token-id, owner: owner })))

(define-read-only (get-recognized-losses (token-id uint) (owner principal))
  (default-to u0 (map-get? recognized-losses { token-id: token-id, owner: owner })))

;; @desc recognize losses by the caller
;; @param token-id: selected pool id
;; @param caller: caller recognizing losses
;; @returns (response uint uint)
(define-public (recognize-losses (token-id uint) (caller principal))
  (let (
    (losses (recognizable-losses-of-read token-id caller)))
    (try! (is-approved-contract contract-caller))
    (map-set recognized-losses { token-id: token-id, owner: caller } (+ losses (get-recognized-losses token-id caller) losses))
    (ok losses)))

(define-public (recognizable-losses-of (token-id uint) (owner principal))
  (ok (- (accumulative-losses-of token-id owner) (get-recognized-losses token-id owner))))

(define-read-only (recognizable-losses-of-read (token-id uint) (owner principal))
  (- (accumulative-losses-of token-id owner) (get-recognized-losses token-id owner)))

(define-read-only (accumulative-losses-of (token-id uint) (owner principal))
  (/
    (to-uint (+
      (to-int (* (get-losses-per-share token-id) (get-balance-uint token-id owner)))
      (get-losses-correction token-id owner)
    ))
  POINTS_MULTIPLIER))

;; @desc distribute points for losses to a pool
;; @param token-id: selected pool id
;; @param delta: amount of losses
;; @returns (response uint uint)
(define-public (distribute-losses (token-id uint) (delta uint))
  (let (
    (total-supply (get-total-supply-uint token-id))
    (valid-token-id (asserts! (> total-supply u0) ERR_PANIC))
    (added-losses (/ (* delta POINTS_MULTIPLIER) total-supply))
    (total-losses-shared (+ (get-losses-per-share token-id) added-losses)))
    (try! (is-approved-contract contract-caller))
    (map-set losses-per-share token-id total-losses-shared)
    (print { type: "lost_funds", added-points: added-losses })
    (ok total-losses-shared)))

;; --- approved
(define-map approved-contracts principal bool)

(define-read-only (is-approved-contract (contract principal))
  (if (default-to false (map-get? approved-contracts contract))
    (ok true)
    ERR_UNAUTHORIZED))

;; -- ownable-trait --
(define-public (get-contract-owner)
  (ok (var-get contract-owner)))

(define-public (set-contract-owner (owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (ok (var-set contract-owner owner))))

(define-read-only (is-contract-owner (caller principal))
  (is-eq caller (var-get contract-owner)))

;; -- pool rewards storage
;; token-id -> cycle-start
(define-map cycle-start uint uint)
;; total rewards in cycle
(define-map rewards { token-id: uint, cycle: uint} uint)

(define-map funds-sent-cycle { token-id: uint, cycle: uint} uint )
(define-map funds-sent-cycle-by-principal { token-id: uint, cycle: uint, user: principal } uint )

(define-constant MAX_REWARD_CYCLES u32)
(define-constant REWARD_CYCLE_INDEXES (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31))

(define-read-only (get-cycle-start (token-id uint))
  (unwrap-panic (map-get? cycle-start token-id)))

(define-read-only (get-cycle-rewards (token-id uint) (cycle uint))
  (default-to u0 (map-get? rewards { token-id: token-id, cycle: cycle })))

(define-read-only (get-cycle-share (token-id uint) (cycle uint))
  (default-to u0 (map-get? funds-sent-cycle { token-id: token-id, cycle: cycle })))

(define-read-only (get-cycle-share-principal (token-id uint) (cycle uint) (user principal))
  (default-to u0 (map-get? funds-sent-cycle-by-principal { token-id: token-id, cycle: cycle, user: user })))

;; @desc delete the cycle commitment between the cycle at which funds were sent and the end of the user's commitment
;; @restricted pool
;; @param token-id: pool id
;; @param caller: user whose map is being cleared
;; @returns (response true uint)
(define-public (empty-commitments (token-id uint) (caller principal))
  (let (
    (funds-sent (unwrap-panic (contract-call? .cover-pool-data get-sent-funds caller token-id)))
    (current-cycle (unwrap-panic (get-current-cycle token-id)))
    (start-cycle (unwrap-panic (get-cycle-at token-id (get start funds-sent))))
    (end-cycle (+ (get cycles funds-sent) start-cycle))
    (n-cycles (- end-cycle start-cycle))
    (total-rewards (fold empty-commitments-clojure REWARD_CYCLE_INDEXES { token-id: token-id, first-cycle: start-cycle, period: n-cycles, caller: caller })))
    (try! (is-approved-contract contract-caller))
    (ok true)))

;; @desc delete the cycle commitment of the selected user in the selected pool and cycle
;; @param cycle-idx: offset integer cycling from [0-31]
;; @param result:
;;  token-id: selected pool-id
;;  first-cycle: starting cycle
;;  period: delta between starting cycle and ending cycle
;;  caller: user whose commitments are being emptied
;; @param caller: user whose map is being cleared
;; @returns (response { token-id: uint, first-cycle: uint, period: uint, caller: principal } uint)
(define-private (empty-commitments-clojure (cycle-idx uint) (result { token-id: uint, first-cycle: uint, period: uint, caller: principal }))
  (let (
    (current-cycle (+ cycle-idx (get first-cycle result)))
    (caller (get caller result))
    (token-id (get token-id result))
    (rewards-in-cycle (get-cycle-rewards token-id current-cycle))
    (share-in-cycle (get-cycle-share token-id current-cycle))
    (funds-sent-in-cycle (get-cycle-share-principal token-id current-cycle caller)))
    (if  (> funds-sent-in-cycle u0)
      (let (
        (portion (/ (* rewards-in-cycle funds-sent-in-cycle) share-in-cycle)))
        ;; add to cycle amounts sent
        (map-delete funds-sent-cycle-by-principal { token-id: token-id, cycle: current-cycle, user: caller })
        ;; add to funds sent by the user
        { token-id: (get token-id result), first-cycle: (get first-cycle result), period: (get period result), caller: caller })
      ;; do if above cycle commitment
      { token-id: (get token-id result), first-cycle: (get first-cycle result), period: (get period result), caller: caller })))

;; @desc remove amount committed in the cycles in [start-cycle, current-cycle[ and return reduced contents
;; @param start-cycle: cycle at which removing starts
;; @param end-cycle: last cycle to be removed
;; @param token-id: selected pool id
;; @param caller: user whose map is being cleared
;; @returns (response uint uint)
(define-private (remove-share-cycle (start-cycle uint) (end-cycle uint) (token-id uint) (caller principal))
  (let (
    (n-cycles (- end-cycle start-cycle))
    (total-rewards (fold remove-share-cycles-clojure REWARD_CYCLE_INDEXES { token-id: token-id, first-cycle: start-cycle, period: n-cycles, total: u0, caller: caller })))
    (print { token-id: token-id, first-cycle: start-cycle, period: n-cycles })
    (ok (get total total-rewards))))

;; @desc remove amount committed in the cycles in [start-cycle, current-cycle[
;; @param cycle-idx: offset integer cycling from [0-31]
;; @param result:
;;  token-id: selected pool-id
;;  first-cycle: starting cycle
;;  period: delta between starting cycle and ending cycle
;;  caller: user whose commitments are being emptied
;; @returns (response { token-id: uint, first-cycle: uint, period: uint, total: uint, caller: principal } uint)
(define-private (remove-share-cycles-clojure (cycle-idx uint) (result { token-id: uint, first-cycle: uint, period: uint, total: uint, caller: principal }))
  (let (
    (current-cycle (+ cycle-idx (get first-cycle result)))
    (caller (get caller result))
    (token-id (get token-id result))
    (rewards-in-cycle (get-cycle-rewards token-id current-cycle))
    (share-in-cycle (get-cycle-share token-id current-cycle))
    (funds-sent-in-cycle (get-cycle-share-principal token-id current-cycle caller)))
    (if (and (> (unwrap-panic (get-current-cycle (get token-id result))) current-cycle) (> funds-sent-in-cycle u0))
      (let (
        (portion (/ (* rewards-in-cycle funds-sent-in-cycle) share-in-cycle)))
        ;; add to cycle amounts sent
        (map-delete funds-sent-cycle-by-principal { token-id: token-id, cycle: current-cycle, user: caller })
        ;; add to funds sent by the user
        { token-id: (get token-id result), first-cycle: (get first-cycle result), period: (get period result), total: (+ (get total result) portion), caller: caller })
      ;; do if above cycle commitment
      { token-id: (get token-id result), first-cycle: (get first-cycle result), period: (get period result), total: (get total result), caller: caller })))

;; @desc add the rewards set in the cycles in [start-cycle, end-cycle] that recipient can earn
;; @param start-cycle: first cycle to get
;; @param end-cycle: last cycle to get
;; @param token-id: pool id
;; @param recipient: user whose rewards are being added up
;; @returns (response uint uint)
(define-read-only (get-sum-cycles (start-cycle uint) (end-cycle uint) (token-id uint) (recipient principal))
  (let (
    (ok-result (asserts! (> end-cycle start-cycle) ERR_NOT_ENOUGH_TIME_PASSED))
    (n-cycles (- end-cycle start-cycle))
    (total-rewards (fold get-sum-cycles-clojure REWARD_CYCLE_INDEXES { token-id: token-id, first-cycle: start-cycle, period: n-cycles, sum: u0, recipient: recipient })))
    (asserts! (<= n-cycles MAX_REWARD_CYCLES) ERR_INVALID_LENGTH)
    (ok (get sum total-rewards))))

;; @desc get the amount in the selected cycle for the recipient
;; @param cycle-idx: offset integer cycling from [0-31]
;; @param result:
;;  token-id: selected pool-id
;;  first-cycle: starting cycle
;;  period: delta between starting cycle and ending cycle
;;  sum: sum of rewards
;;  recipient: user whose commitments are being added up
;; @returns (response { token-id: uint, first-cycle: uint, period: uint, sum: uint, recipient: uint } uint)
(define-private (get-sum-cycles-clojure (cycle-idx uint) (result { token-id: uint, first-cycle: uint, period: uint, sum: uint, recipient: principal }))
  (let (
    (current-cycle (+ cycle-idx (get first-cycle result)))
    (token-id (get token-id result))
    (rewards-in-cycle (get-cycle-rewards token-id current-cycle))
    (share-in-cycle (get-cycle-share token-id current-cycle))
    (funds-sent-in-cycle (get-cycle-share-principal token-id current-cycle (get recipient result))))
    (if (and (> (unwrap-panic (get-current-cycle (get token-id result))) current-cycle) (> funds-sent-in-cycle u0))
      (let (
        (portion (/ (* rewards-in-cycle funds-sent-in-cycle) share-in-cycle)))
        { token-id: (get token-id result), first-cycle: (get first-cycle result), period: (get period result), sum: (+ portion (get sum result)), recipient: (get recipient result) })
      ;; do if all cycles
      { token-id: (get token-id result), first-cycle: (get first-cycle result), period: (get period result), sum: (get sum result), recipient: (get recipient result) })))

;; @desc set amount committed to the cycles map of the caller to the cycles in ]start-cycle,end-cycle]
;; @param start-cycle: cycle at which removing starts
;; @param end-cycle: last cycle to be removed
;; @param token-id: selected pool id
;; @param amount: amount being set
;; @param caller: user whose map is being set
;; @returns (response true uint)
(define-public (set-share-cycles (start-cycle uint) (end-cycle uint) (token-id uint) (amount uint) (caller principal))
  (let (
    (n-cycles (- end-cycle start-cycle))
    (total-rewards (fold set-share-cycles-clojure REWARD_CYCLE_INDEXES { token-id: token-id, first-cycle: (+ u1 start-cycle), period: n-cycles, amount: amount, caller: caller })))
    (try! (is-approved-contract contract-caller))
    (asserts! (<= n-cycles MAX_REWARD_CYCLES) ERR_INVALID_LENGTH)

    (print { token-id: token-id, first-cycle: (+ u1 start-cycle), period: n-cycles, amount: amount })
    (ok true)))

;; @desc set amount committed to the cycle map of the caller to the cycles in ]first-cycle,end-cycle]
;; @param cycle-idx: offset integer cycling from [0-31]
;; @param result:
;;  token-id: selected pool-id
;;  first-cycle: starting cycle
;;  period: delta between starting cycle and ending cycle
;;  amount: amount being set
;;  caller: user whose commitments are being added
;; @returns (response { token-id: uint, first-cycle: uint, period: uint, amount: uint, caller: principal } uint)
(define-private (set-share-cycles-clojure (cycle-idx uint) (result { token-id: uint, first-cycle: uint, period: uint, amount: uint, caller: principal }))
  (let (
    (caller (get caller result))
    (share-in-cycle (get-cycle-share (get token-id result) (+ cycle-idx (get first-cycle result))))
    (funds-sent-in-cycle (get-cycle-share-principal (get token-id result) (+ cycle-idx (get first-cycle result)) caller)))
    (if (> (get period result) cycle-idx)
      (begin
        ;; add to cycle amounts sent
        (map-set funds-sent-cycle { token-id: (get token-id result), cycle: (+ cycle-idx (get first-cycle result)) } (+ (get amount result) share-in-cycle))
        ;; add to funds sent by the user
        (map-set funds-sent-cycle-by-principal { token-id: (get token-id result), cycle: (+ cycle-idx (get first-cycle result)), user: caller } (+ funds-sent-in-cycle (get amount result)))
        { token-id: (get token-id result), first-cycle: (get first-cycle result), period: (get period result), amount: (get amount result), caller: caller })
      ;; do if above cycle commitment
      { token-id: (get token-id result), first-cycle: (get first-cycle result), period: (get period result), amount: (get amount result), caller: caller })))

;; @desc set the start of the cycle for a token-id
;; @restricted pool
;; @param token-id: pool id
;; @param start: start of the cycle
;; @returns (respose true uint)
(define-public (set-cycle-start (token-id uint) (start uint))
  (begin
    (try! (is-approved-contract contract-caller))
    (ok (map-set cycle-start token-id start))))

(define-public (get-committed-funds (token-id uint) (owner principal))
  (ok (/ (get-balance-uint token-id owner) DFT_PRECISION)))

(define-read-only (get-cover-pool-sent-funds (token-id uint) (sender principal))
  (get-balance-uint token-id sender))

(define-read-only (get-cover-pool-lost-funds (token-id uint) (sender principal))
  (recognizable-losses-of-read token-id sender))

(define-read-only (get-cover-pool-funds-balance (token-id uint) (sender principal))
  (let (
    (sent-funds (get-cover-pool-sent-funds token-id sender))
    (lost-funds (get-cover-pool-lost-funds token-id sender)))
    (- sent-funds lost-funds)))

(map-set approved-contracts .payment-fixed true)
(map-set approved-contracts .pool-v1-0 true)
(map-set approved-contracts .cover-pool-v1-0 true)

;; ERROR START 11000
(define-constant ERR_INVALID_PRINCIPAL (err u11000))
(define-constant ERR_INSUFFICIENT_BALANCE (err u11001))
(define-constant ERR_PANIC (err u11002))

(define-constant ERR_INVALID_LENGTH (err u11003))
(define-constant ERR_NOT_ENOUGH_TIME_PASSED (err u11004))