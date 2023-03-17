;; mini sBTC skeleton
;;
;; What's here:
;; * The signer registration process
;; * The PoX reward disbursement process, minus the bitcoin transaction processing
;;
;; What's not here:
;; * BTC wraps
;; * BTC unwraps
;; * BTC wallet hand-off
;; * The penalty window
;;

;; Errors
(define-constant ERR-MISSING-DISBURSEMENTS u1)
(define-constant ERR-TX-ALREADY-PROCESSED u2)
(define-constant ERR-INVALID-BITCOIN-TX u3)
(define-constant ERR-INVALID-DISBURSEMENT-HASH u4)

(define-constant ERR-NOT-REGISTRATION-WINDOW u1000)
(define-constant ERR-ALREADY-REGISTERED u1001)
(define-constant ERR-SBTC-NOT-AUTHORIZED-STACKER-DELEGATE u1002)
(define-constant ERR-INSUFFICIENT-UNLOCK-BALANCE u1003)
(define-constant ERR-SIGNING-KEY-ALREADY-USED u1004)
(define-constant ERR-NOT-A-STACKER u1005)
(define-constant ERR-STACKER-DOES-NOT-UNLOCK u1005)
(define-constant ERR-STACKER-NOT-QUALIFIED u1006)
(define-constant ERR-STACKER-ALREADY-PRE-REGISTERED u1007)
(define-constant ERR-PAYOUT-ADDRESS-ALREADY-USED u1008)

(define-constant ERR-NOT-VOTING-WINDOW u2000)
(define-constant ERR-SIGNER-NOT-REGISTERED u2002)
(define-constant ERR-SIGNER-ALREADY-VOTED u2003)
(define-constant ERR-NOT-ENOUGH-STX u2004)

(define-constant ERR-NO-WINNING-POX-ADDR u3000)
(define-constant ERR-FUTURE-REWARD-CYCLE u3001)
(define-constant ERR-PAYOUT-NOT-CALCULATED u3002)
(define-constant ERR-PAYOUT-ALREADY-MADE u3003)
(define-constant ERR-NOT-A-SIGNER u3004)

;; Same burnchain and PoX constants as mainnet
(define-data-var first-burn-block-height uint u666050)
(define-data-var reward-cycle-len uint u2100)

;; Relative burnchain block heights (between 0 and 2100) as to when the system transitions into different states
(define-data-var registration-window-rel-end uint u1600)
(define-data-var voting-window-rel-end uint u1900)
(define-data-var transfer-window-rel-end uint u2000)
(define-data-var penalty-window-rel-end uint u2100)

;; A list of 100 entries (used for iteration)
(define-constant LIST-100 (list
   true true true true true true true true true true 
   true true true true true true true true true true 
   true true true true true true true true true true 
   true true true true true true true true true true 
   true true true true true true true true true true 
   true true true true true true true true true true 
   true true true true true true true true true true 
   true true true true true true true true true true 
   true true true true true true true true true true 
   true true true true true true true true true true))

;; Minimum number of uSTX required to be a signer (1,000 STX)
;; This is used in place of the stacking minimum, which we don't have access to at the time of registration.
;; Set this to whatever is appropriate.
(define-data-var ustx-minimum uint u100000000)

;; All info for the active signers, grouped by reward cycle
(define-map signer-state
    { signer: principal, reward-cycle: uint }
    {
        ;; amount stacked
        amount-ustx: uint,
        ;; signer public key
        public-key: (buff 33),
        ;; calculated lazily on disbursement
        btc-earned: (optional uint),
        ;; Bitcoin address to which PoX rewards will be sent.
        ;; Must be unique for this reward cycle.
        payout-addr: { version: (buff 1), hashbytes: (buff 32) },
        ;; this signer's vote for the sBTC wallet address in this cycle
        pox-addr-vote: (optional { version: (buff 1), hashbytes: (buff 32) }),
    }
)

;; All info for pre-registered signers who are currently stacking, and will
;; be allowed to register in the next reward cycle
(define-map pre-signer-state
    { signer: principal, reward-cycle: uint }
    ;; no-op value
    bool
)

;; All public keys ever used, so we don't re-use them
(define-map signer-public-keys-used
    ;; public key
    (buff 33)
    ;; no-op
    bool
)

;; All payout addresses used in a reward cycle, so we don't reuse them
(define-map payout-addrs-in-reward-cycle
    ;; pox addr
    { version: (buff 1), hashbytes: (buff 32) }
    ;; reward cycle
    uint
)

;; STX stacked per payout address in a reward cycle
(define-map payout-addr-state
    { reward-cycle: uint, payout-addr: { version: (buff 1), hashbytes: (buff 32) } }
    {
        ;; amount of locked uSTX this address represents
        amount-ustx: uint,
        ;; back-ptr to signer-state
        signer: principal
    }
)

;; default value for the above map
(define-constant DEFAULT-PAYOUT-ADDR-STATE { amount-ustx: u0, signer: 'ST000000000000000000002AMW42H })

;; voted-upon PoX addresses by signers
(define-map voted-pox-addrs
    { reward-cycle: uint, pox-addr: { version: (buff 1), hashbytes: (buff 32) } }
    {
        ;; aggregate-commit index for this address
        agg-index: (optional uint),
        ;; total ustx represented
        total-ustx: uint,
        ;; total number of signers
        num-signers: uint,
    }
)

;; PoX addresses used for pre-registration.
(define-map pre-register-pox-addrs
    { reward-cycle: uint, pox-addr: { version: (buff 1), hashbytes: (buff 32) } }
    {
        ;; aggregate-commit index for this address
        agg-index: (optional uint),
        ;; total ustx represented
        total-ustx: uint,
        ;; total number of signers
        num-signers: uint,
    }
)

;; winner PoX addresses in each reward cycle.
;; Instantiated during the voting period for the given reward cycle, and then never touched again.
(define-map winning-pox-addrs
    ;; reward cycle
    uint
    ;; address
    { version: (buff 1), hashbytes: (buff 32) }
)

(define-constant DEFAULT-WINNING-POX-ADDR { version: 0x00, hashbytes: 0x0000000000000000000000000000000000000000000000000000000000000000 })

;; Amount of BTC accrued to the winning PoX address.
;; Iteratively updated for a given reward cycle by scanning the PoX payouts for each block, and summing up
;; the BTC paid to the given address.
;; Instantiated during the disbursement period, and then never touched again once all disbursements for the cycle complete
(define-map disbursed-btc-state
    ;; reward-cycle
    uint
    {
        ;; PoX address (same as sBTC wallet address; same as address entry in `winning-pox-addrs`
        addr: { version: (buff 1), hashbytes: (buff 32) },
        ;; amount of BTC accumulated so far
        btc: uint,
        ;; number of blocks in the reward cycle processed so far
        blocks: uint
    }
)

;; Disbursement-processing state per reward cycle.
;; Gets updated when a disbursement transaction gets processed.
(define-map disbursement-state
    ;; reward cycle
    uint
    {
        ;; total uSTX locked up for this cycle
        total-ustx: uint,
        ;; number of payout addresses in this cycle (note that they are all unique)
        num-btc-addrs: uint,
        ;; number of paid-out addresses in this cycle.  Once this value is equal to `num-btc-addrs`, then 
        ;; all disbursements for this cycle will have been made.
        num-disbursed-addrs: uint
    }
)

(define-constant DEFAULT-DISBURSEMENT-STATE { total-ustx: u0, num-btc-addrs: u0, num-disbursed-addrs: u0 })

;; Disbursement rolling hash calculation per reward cycle.
;; Used to generate a key that prevents backdated transactions from being used as proofs of disbursement
(define-map disbursement-hash
    ;; reward cycle
    uint
    ;; the hash
    (buff 32)
)

;; Highest reward cycle in which all rewards are disbursed
(define-data-var highest-disbursed-reward-cycle uint u0)

;; Transactions we've seen, so we don't process them again
(define-map processed-txids
    (buff 32)
    ;; no-op
    bool
)

;; Check that this contract is authorized as the stacker in .pox-2
(define-read-only (is-authorized-as-stacker (stacker principal)
                                            (burn-block-ht uint))
    (match (contract-call? 'ST000000000000000000002AMW42H.pox-2 get-allowance-contract-callers stacker (as-contract tx-sender))
        allowance-record
            (match (get until-burn-ht allowance-record)
                until-ht
                    (> burn-block-ht until-ht)
                true)
        false))

;; Delegate and stack a new signer's STX to an address.
;; Does not constitute a vote if this is the signer's first time.
;; The PoX address is the stacker's own address; it's not a FROST-derived address.
(define-private (stack-new-signer-ustx (signer principal)
                                       (amount-ustx uint)
                                       (pox-addr { version: (buff 1), hashbytes: (buff 32) })
                                       (cur-burn-ht uint))
    (let (
        (rc (+ u1 (get-reward-cycle cur-burn-ht)))
        (until-burn-ht (+ u1 (var-get first-burn-block-height) (* (var-get reward-cycle-len) rc)))
        (pre-register-rec-opt (map-get? pre-register-pox-addrs { reward-cycle: rc, pox-addr: pox-addr }))
    )
        ;; give this contract the ability to stack these STX
        (unwrap-panic (contract-call? 'ST000000000000000000002AMW42H.pox-2 delegate-stx amount-ustx (as-contract tx-sender) (some until-burn-ht) (some pox-addr)))
        
        ;; have the contract go stack them
        (as-contract
            (unwrap-panic
                (contract-call? 'ST000000000000000000002AMW42H.pox-2 delegate-stack-stx signer amount-ustx pox-addr cur-burn-ht u1)))

        ;; aggregate-commit (or increase) the STX locked to this PoX address,
        ;; but don't count this as a vote for this address.
        (match pre-register-rec-opt
            ;; existing PoX record, so go increase the commit record
            pre-register-rec
                (begin
                   (as-contract
                       (unwrap-panic
                           (contract-call? 'ST000000000000000000002AMW42H.pox-2 stack-aggregation-increase pox-addr rc (unwrap-panic (get agg-index pre-register-rec)))))
                    pre-register-rec
                )

            ;; no PoX record, so go and aggregate-commit for this new one 
            {
                agg-index: (some (as-contract
                    (unwrap-panic (contract-call? 'ST000000000000000000002AMW42H.pox-2 stack-aggregation-commit-indexed pox-addr rc)))),
                total-ustx: u0,
                num-signers: u0
            })
    ))

;; Delegate and stack the recurring signer's STX to an address.
;; Calculates and returns a new vote record for `voted-pox-addrs` for this (reward-cycle, pox-addr) pair.
;; pox-addr is the FROST address candidate.
(define-private (stack-returning-signer-ustx (signer principal)
                                             (amount-ustx uint)
                                             (pox-addr { version: (buff 1), hashbytes: (buff 32) })
                                             (cur-burn-ht uint))
    (let (
        (rc (+ u1 (get-reward-cycle cur-burn-ht)))
        (until-burn-ht (+ u1 (var-get first-burn-block-height) (* (var-get reward-cycle-len) rc)))
        (vote-rec-opt (map-get? voted-pox-addrs { reward-cycle: rc, pox-addr: pox-addr }))
    )
        ;; give this contract the ability to stack these STX
        (unwrap-panic (contract-call? 'ST000000000000000000002AMW42H.pox-2 delegate-stx amount-ustx (as-contract tx-sender) (some until-burn-ht) (some pox-addr)))

        ;; have the contract go re-stack them
        (as-contract
            (unwrap-panic
                (contract-call? 'ST000000000000000000002AMW42H.pox-2 delegate-stack-extend signer pox-addr u1)))

        ;; aggregate-commit (or increase) the STX locked to this PoX address,
        ;; and record how much uSTX is behind this PoX address for signing
        (match vote-rec-opt
            ;; existing voted-upon PoX record, so go increase the commit record
            vote-rec
                (begin
                    (as-contract
                        (unwrap-panic
                            (contract-call? 'ST000000000000000000002AMW42H.pox-2 stack-aggregation-increase pox-addr rc (unwrap-panic (get agg-index vote-rec)))))
                    {
                        agg-index: (get agg-index vote-rec),
                        total-ustx: (+ amount-ustx (get total-ustx vote-rec)),
                        num-signers: (+ u1 (get num-signers vote-rec))
                    })

            ;; no voted-upon PoX record, so go and aggregate-commit for this new one 
            {
                agg-index: (some (as-contract
                    (unwrap-panic (contract-call? 'ST000000000000000000002AMW42H.pox-2 stack-aggregation-commit-indexed pox-addr rc)))),
                total-ustx: amount-ustx,
                num-signers: u1
            })
    ))

;; Get the reward cycle for a given burn block height.
;; Runtime-panics if it's before first-burn-block-height.
(define-read-only (get-reward-cycle (burn-block-ht uint))
    (/ (- burn-block-ht (var-get first-burn-block-height)) (var-get reward-cycle-len)))

;; Get the relative burn height -- the height of the burnchain within this reward cycle.
(define-read-only (get-rel-burn-height (burn-block-ht uint))
    (mod burn-block-ht (var-get reward-cycle-len)))

;; Have all funds been disbursed from the last reward cycle?
(define-read-only (all-rewards-disbursed (burn-ht uint))
    (let (
        (rc (get-reward-cycle burn-ht))
        (disburse-info (default-to DEFAULT-DISBURSEMENT-STATE (map-get? disbursement-state rc)))
        (highest-disbursed-rc (var-get highest-disbursed-reward-cycle))
    )
        (if (< rc highest-disbursed-rc)
            ;; already disbursed
            true
            ;; not yet known to be disbursed. check the number of times a successful disbursement happened
            (is-eq (get num-btc-addrs disburse-info) (get num-disbursed-addrs disburse-info)))
    ))

;; Bump disbursement requirements for a given reward cycle.
;; Called each time a new signer registers.
(define-private (bump-disbursement-requirements (rc uint) (new-ustx uint))
    (let (
        (disbursement-info (default-to DEFAULT-DISBURSEMENT-STATE (map-get? disbursement-state rc)))
    )
        (map-set disbursement-state
            rc
            (merge disbursement-info {
                total-ustx: (+ new-ustx (get total-ustx disbursement-info)),
                num-btc-addrs: (+ u1 (get num-btc-addrs disbursement-info)),
            }))
    ))

;; Update the PoX payout record by up to 100 burnchain blocks.
;; Used within a (fold ...)
(define-private (iter-update-btc-payout (ignored bool)
                                        (state {
                                            addr: { version: (buff 1), hashbytes: (buff 32) },
                                            btc: uint,
                                            blocks: uint,
                                            rc: uint,
                                            burn-ht: uint
                                         }))
    (let (
        (rc-len (var-get reward-cycle-len))
        (rc-start-block (+ u1 (var-get first-burn-block-height) (* rc-len (get rc state))))
        (check-block (+ rc-start-block (get blocks state)))
        (reward-addrs-opt (get-burn-block-info? pox-addrs check-block))

        ;; only process a payout if:
        ;;  * there are still unprocessed payouts in this reward cycle.
        ;;  * the burnchain block we're about to query has already been processed.
        (payouts-available?
            (and
                (< (get blocks state) rc-len)
                (< check-block (get burn-ht state))))

        (btc-payout (match reward-addrs-opt
            reward-addrs
                (if payouts-available?
                   ;; have reward addresses for this block, and we're not done tabulating. Sum up the payouts for the ones that math state.addr
                   (+
                       (if (is-eq (element-at (get addrs reward-addrs) u0) (some (get addr state)))
                           (get payout reward-addrs)
                           u0
                       )
                       (if (is-eq (element-at (get addrs reward-addrs) u1) (some (get addr state)))
                           (get payout reward-addrs)
                           u0
                       )
                   )
                   ;; have already completed tabulation
                   u0)
            ;; no reward addresses for this block
            u0))
    )
        (merge state {
            btc: (+ (get btc state) btc-payout),
            blocks: (if payouts-available?
                        (+ u1 (get blocks state))
                        (get blocks state)),
        }) 
    ))

;; Tabulate how much BTC was won through PoX for the next 100 blocks.
;; Returns that tabulation over the next 100 blocks, as of the record stored in `disbursed-btc-state`
;; TODO: some benchmarking on this method will be needed to determine how much compute resources it uses.
;; It may need to be reduced (or increased!).
(define-read-only (get-next-btc-payout (rc uint) (burn-ht uint))
    (let (
        (pox-rec-opt (map-get? disbursed-btc-state rc))
    )
        (match pox-rec-opt
            pox-rec
                ;; already begun calculating this
                (ok (fold iter-update-btc-payout LIST-100 (merge pox-rec { rc: rc, burn-ht: burn-ht })))
            ;; have not started
            (let (
                (winning-pox-addr (unwrap! (map-get? winning-pox-addrs rc) (err ERR-NO-WINNING-POX-ADDR)))
            )
                (ok (fold iter-update-btc-payout LIST-100
                    {
                        addr: winning-pox-addr,
                        btc: u0,
                        blocks: u0,
                        rc: rc,
                        burn-ht: burn-ht
                    }))
            ))
    ))

;; Can the BTC tabulation be updated?
(define-private (can-update-btc-payout (rc uint) (burn-ht uint))
    (let (
        (rc-len (var-get reward-cycle-len))
        (rc-start-block (+ u1 (var-get first-burn-block-height) (* rc-len rc)))
    )
        (asserts! (>= rc-start-block burn-ht)
            (err ERR-FUTURE-REWARD-CYCLE))

        (ok true)
    ))

;; Carry out the tabulation update.
;; Does not check validity of rc
(define-private (inner-update-btc-payout (rc uint) (burn-ht uint))
    (let (
        (updated-tabulation (try! (get-next-btc-payout rc burn-ht)))
    )
        (map-set disbursed-btc-state rc {
            addr: (get addr updated-tabulation),
            btc: (get btc updated-tabulation),
            blocks: (get blocks updated-tabulation)
        })
        (ok true)
    ))

;; Update the disbursed BTC state.  Pool participants call this regularly so that this contract tracks disbursement.
;; Can be called for any current or past reward cycle
(define-public (update-btc-payout (rc uint))
    (begin
        (try! (can-update-btc-payout rc burn-block-height))
        (unwrap-panic (inner-update-btc-payout rc burn-block-height))
        (ok true)
    )) 

;; Calculate how much BTC is owed to a given payout address in a given reward cycle.
;; The total BTC payout for the given reward cycle must already have been calculated.
(define-read-only (get-btc-owed (rc uint) (signer principal))
    (let (
        (disbursement-rec (default-to DEFAULT-DISBURSEMENT-STATE (map-get? disbursement-state rc)))
        (signer-rec (unwrap! (map-get? signer-state { signer: signer, reward-cycle: rc })
            (err ERR-NOT-A-SIGNER)))
        (tabulation (unwrap! (map-get? disbursed-btc-state rc)
            (err ERR-PAYOUT-NOT-CALCULATED)))
    )
        ;; must have completed tabulation
        (asserts! (is-eq (get blocks tabulation) (var-get reward-cycle-len))
            (err ERR-PAYOUT-NOT-CALCULATED))

        (ok (if (> (get total-ustx disbursement-rec) u0)
            ;; some uSTX stacked. so calculate pro-rata share
            (/ (* (get amount-ustx signer-rec) (get btc tabulation)) (get total-ustx disbursement-rec))
            ;; no uSTX stacked
            u0
        ))
    ))

;; Mark a signer as having received their pro-rata share of the BTC for a given reward cycle.
;; Returns (ok true) if all payout addresses have been disbursed
;; Returns (ok false) if not, but we successfully recorded that this signer got paid.
;; Returns (err ...) otherwise.
(define-private (inner-mark-disbursed (rc uint) (signer principal) (amount-btc uint))
    (let (
        (signer-rec (unwrap! (map-get? signer-state { signer: signer, reward-cycle: rc })
            (err ERR-NOT-A-SIGNER)))
        (disbursement-rec (unwrap! (map-get? disbursement-state rc)
            (err ERR-FUTURE-REWARD-CYCLE)))
    )
        (asserts! (is-none (get btc-earned signer-rec))
            (err ERR-PAYOUT-ALREADY-MADE))

        ;; this signer got this BTC
        (map-set signer-state
            { signer: signer, reward-cycle: rc }
            (merge signer-rec { btc-earned: (some amount-btc) }))

        ;; bump number of disbursements made
        (map-set disbursement-state rc
            (merge disbursement-rec { num-disbursed-addrs: (+ u1 (get num-disbursed-addrs disbursement-rec)) }))

        ;; are we done disbursing?
        ;; return true if so; false if not
        (ok (if (is-eq (+ u1 (get num-disbursed-addrs disbursement-rec)) (get num-btc-addrs disbursement-rec))
            (let (
                (highest-disbursed-rc (var-get highest-disbursed-reward-cycle))
            )
                (if (> rc highest-disbursed-rc)
                    ;; returns true
                    (var-set highest-disbursed-reward-cycle rc)
                    false)
            )
            false
        ))
    ))

;; Verify that a Bitcoin transaction was mined on the Bitcoin chain
;; Returns (ok true) if so.
;; Returns (err ...) if not.
;; TODO: implement this
(define-read-only (authenticate-bitcoin-tx (burn-ht uint)
                                           (tx (buff 1024))
                                           (btc-header (buff 80))
                                           (proof {
                                              tx-index: uint,
                                              hashes: (list 14 (buff 32)),
                                              tree-depth: uint
                                           }))
    ;; TODO: use clarity-bitcoin.clar or the like
    ;; give a full error type so the type-checker is happy
    (if (is-eq u0 (mod burn-ht u2))
        (err ERR-INVALID-BITCOIN-TX)
        (ok true)))
    

;; Decode a raw Bitcoin transaction to extract its disbursement outputs.
;; Returns a list of up to 16 recipients and the BTC they each received.
;; TODO: implement this
(define-read-only (decode-disbursement-tx (tx (buff 1024)) (reward-cycle uint))
    (let (
        (dh (default-to 0x0000000000000000000000000000000000000000000000000000000000000000
          (map-get? disbursement-hash reward-cycle)))
        (first-reward-cycle-block-height (+ (var-get first-burn-block-height) (* (var-get reward-cycle-len) (+ u1 reward-cycle))))
        (btc-block-hash (unwrap-panic (get-burn-block-info? header-hash first-reward-cycle-block-height)))

        ;; TODO: get the OP_RETURN's disbursement-hash from the transaction
        (op-return-dh 0x0000000000000000000000000000000000000000000000000000000000000000)
    )
        (asserts! (is-eq (sha512/256 (concat dh btc-block-hash)) op-return-dh)
            (err ERR-INVALID-DISBURSEMENT-HASH))

        ;; TODO: actually decode the tx
        (ok (list
            { btc: u5500,  addr: { version: 0x00, hashbytes: 0x0101010101010101010101010101010101010101 } }
            { btc: u11000, addr: { version: 0x06, hashbytes: 0x0707070707070707070707070707070707070707070707070707070707070707 } }
        ))
    ))

;; Mark all of a disbursement transaction's recipients as having been paid, provided that they paid the right amount.
;; It's okay if some of the outputs are invalid.
;; When finished, the `state.succeeded-bits` tuple member will contain a bitmap of which txouts were valid.  If the ith
;; payout was valid, then the ith bit will be set.
(define-private (iter-process-disbursements (output { btc: uint, addr: { version: (buff 1), hashbytes: (buff 32) } })
                                            (state { rc: uint, succeeded-bits: uint, cnt: uint }))
    (let (
        (payout-rec-opt (map-get? payout-addr-state { reward-cycle: (get rc state), payout-addr: (get addr output) }))
        (succeeded
            (match payout-rec-opt
                payout-rec
                    (let (
                        ;; should not fail -- each payout address is paired with a signer on registration
                        (payout-owed (unwrap-panic (get-btc-owed (get rc state) (get signer payout-rec))))
                    )
                    (if (<= payout-owed (get btc output))
                        ;; this payout address got the BTC it was owed, so go ahead and mark it disbursed.
                        ;; it's okay if this fails -- absorb the error and just don't set the bit
                        (match (inner-mark-disbursed (get rc state) (get signer payout-rec) (get btc output))
                            ok-res (bit-or (get succeeded-bits state) (bit-shift-left u1 (get cnt state)))
                            err-res (get succeeded-bits state))
                        (get succeeded-bits state)
                    ))
                (get succeeded-bits state)))
    )
        (merge state { succeeded-bits: succeeded, cnt: (+ u1 (get cnt state)) })
    ))

;; Given a disbursement transaction for a target reward cycle, a block header, and a block height, go verify that the 
;; disbursement transaction happened.
;; Returns a bitmap of which txouts were treated as disbursements -- the ith bit of the return value is set if the ith
;; txout was processed successfully.
;; TODO: implement this
(define-public (process-disbursement-tx (burn-ht uint)
                                        (tx (buff 1024))
                                        (btc-header (buff 80))
                                        (proof {
                                           tx-index: uint,
                                           hashes: (list 14 (buff 32)),
                                           tree-depth: uint
                                        }))
    (begin
        (try! (authenticate-bitcoin-tx burn-ht tx btc-header proof))
        (let (
            ;; NOTE: this is the _reversed_ txid -- the byte order is the opposite of what you see in a block explorer
            (btc-txid (sha256 (sha256 tx)))
            (for-reward-cycle (get-reward-cycle burn-ht))
            (decoded-disbursements (try! (decode-disbursement-tx tx for-reward-cycle)))
        )
            (asserts! (is-none (map-get? processed-txids btc-txid))
                (err ERR-TX-ALREADY-PROCESSED))

            (map-set processed-txids btc-txid true)

            (ok (get succeeded-bits
                (fold iter-process-disbursements decoded-disbursements { rc: for-reward-cycle, succeeded-bits: u0, cnt: u0 })))
        )
    ))

;; Is a burn block height in the registration window?
(define-read-only (in-registration-window (burn-ht uint))
    (and (all-rewards-disbursed burn-ht)
         (< (get-rel-burn-height burn-ht) (var-get registration-window-rel-end))))

;; Is a burn block height in the voting window?
(define-read-only (in-voting-window (burn-ht uint))
    (let (
        (rel-ht (get-rel-burn-height burn-ht))
    )
        (and (>= rel-ht (var-get registration-window-rel-end))
             (< rel-ht (var-get voting-window-rel-end)))
    ))

;; Is a burn block height in the transfer window?
(define-read-only (in-transfer-window (burn-ht uint))
    (let (
        (rel-ht (get-rel-burn-height burn-ht))
    )
        (and (>= rel-ht (var-get voting-window-rel-end))
             (< rel-ht (var-get transfer-window-rel-end)))
    ))

;; Is a burn block height in the penalty window?
(define-read-only (in-penalty-window (burn-ht uint))
    (>= (get-rel-burn-height burn-ht) (var-get transfer-window-rel-end)))

;; Can a stacker pre-register to be a signer?
(define-read-only (can-signer-pre-register (signer principal)
                                           (amount-ustx uint)
                                           (cur-burn-ht uint))
    (let (
        (account (stx-account signer))
        (cur-rc (get-reward-cycle cur-burn-ht))
        (rc (+ u1 cur-rc))
        (cur-signer-rec-opt (map-get? signer-state { signer: signer, reward-cycle: cur-rc }))
        (cur-pre-signer-rec-opt (map-get? pre-signer-state { signer: signer, reward-cycle: cur-rc }))
        (cur-winning-pox-addr-opt (map-get? winning-pox-addrs cur-rc))
    )
        ;; we're in the registration window
        (asserts! (in-registration-window cur-burn-ht)
            (err ERR-NOT-REGISTRATION-WINDOW))

        ;; the caller authorized this contract as its stacker delegate
        (asserts! (is-authorized-as-stacker signer cur-burn-ht)
            (err ERR-SBTC-NOT-AUTHORIZED-STACKER-DELEGATE))

        ;; the caller is not pre-registered
        (asserts! (is-none cur-pre-signer-rec-opt)
            (err ERR-STACKER-ALREADY-PRE-REGISTERED))

        ;; the caller is either not registered, or did not vote
        (asserts!
            (match cur-signer-rec-opt
                cur-signer-rec
                    ;; currently registered as a signer, but did not vote for the winning sBTC address
                    (or (is-none (get pox-addr-vote cur-signer-rec))
                        (is-eq cur-winning-pox-addr-opt (get pox-addr-vote cur-signer-rec)))
                ;; not registered
                true)
            (err ERR-STACKER-NOT-QUALIFIED))

        ;; signer has enough STX
        (asserts! (>= (get unlocked account) amount-ustx)
            (err ERR-INSUFFICIENT-UNLOCK-BALANCE))
        
        (ok true)
    ))

;; Pre-register the signer: stacks the signer's STX to a PoX address of their choice.
;; This is done to ensure that the signer candidate's STX remain locked between when they
;; register to be a signer in the next reward cycle and when they vote for a sBTC wallet address.
(define-private (inner-signer-pre-register (signer principal)
                                           (amount-ustx uint)
                                           (cur-burn-ht uint)
                                           (pox-addr { version: (buff 1), hashbytes: (buff 32) }))
                    
    (let (
        (rc (+ u1 (get-reward-cycle cur-burn-ht)))
    )
        (let (
            ;; delegate and stack the STX
            (pre-register-pox-addr (stack-new-signer-ustx signer amount-ustx pox-addr cur-burn-ht))
        )
            ;; record metadata so multiple stackers can stack to the same PoX address
            (map-set pre-register-pox-addrs { reward-cycle: rc, pox-addr: pox-addr } pre-register-pox-addr)
        )

        ;; this signer has pre-registered
        (map-set pre-signer-state { signer: signer, reward-cycle: rc } true)
        (ok true)
    ))

;; Pre-register the signer
(define-public (signer-pre-register (amount-ustx uint)
                                    (pox-addr { version: (buff 1), hashbytes: (buff 32) }))
    (begin
        (try! (can-signer-pre-register tx-sender amount-ustx burn-block-height))
        (inner-signer-pre-register tx-sender amount-ustx burn-block-height pox-addr)
    ))

;; Can a stacker register as a signer?
(define-read-only (can-signer-register (signer principal)
                                       (amount-ustx uint)
                                       (public-key (buff 33))
                                       (cur-burn-ht uint)
                                       (payout-addr { version: (buff 1), hashbytes: (buff 32) }))
    (let (
        (account (stx-account signer))
        (cur-rc (get-reward-cycle cur-burn-ht))
        (rc (+ u1 cur-rc))
        (cur-signer-rec-opt (map-get? signer-state { signer: signer, reward-cycle: cur-rc }))
        (cur-pre-signer-rec-opt (map-get? pre-signer-state { signer: signer, reward-cycle: cur-rc }))
        (cur-winning-pox-addr-opt (map-get? winning-pox-addrs cur-rc))
    )
        ;; we're in the registration window
        (asserts! (in-registration-window cur-burn-ht)
            (err ERR-NOT-REGISTRATION-WINDOW))

        ;; the caller authorized this contract as its stacker delegate
        (asserts! (is-authorized-as-stacker signer cur-burn-ht)
            (err ERR-SBTC-NOT-AUTHORIZED-STACKER-DELEGATE))

        ;; the caller's public key is not used yet
        (asserts! (is-none (map-get? signer-public-keys-used public-key))
            (err ERR-SIGNING-KEY-ALREADY-USED))

        ;; the caller's payout address is not used yet
        (asserts!
            (match (map-get? payout-addrs-in-reward-cycle payout-addr)
                payout-addr-rc
                    (not (is-eq payout-addr-rc rc))
                true)
            (err ERR-PAYOUT-ADDRESS-ALREADY-USED))

        ;; this stacker pre-registered in the current reward cycle, OR
        ;; this stacker is a signer in the current reward cycle AND
        ;; voted for the winning PoX address for the current cycle if there was one.
        (asserts!
            (match cur-pre-signer-rec-opt
                cur-pre-signer-rec
                    ;; stacker pre-registered in the current cycle, and is eligible to register for the next
                    true
                (match cur-signer-rec-opt
                    cur-signer-rec
                        ;; stacker is a registered signer right now
                        (if (is-some cur-winning-pox-addr-opt)
                            ;; there's a winning address active, so this stacker must have voted for it to be
                            ;; eligible to register for the next cycle
                            (is-eq cur-winning-pox-addr-opt (get pox-addr-vote cur-signer-rec))
                            ;; no winning address active, meaning this signer did not vote (and must pre-register).
                            ;; therefore, this signer is _not_ eligible to register again.
                            false
                        )
                    ;; stacker is neither registered nor pre-registered
                    false))
            (err ERR-STACKER-NOT-QUALIFIED))

        ;; the signer is currently stacking at least `amount-ustx` STX
        (asserts! (>= (get locked account) amount-ustx)
            (err ERR-NOT-A-STACKER))

        ;; the signer's STX will unlock in the upcoming reward cycle
        (asserts! (is-eq (+ u1 (get-reward-cycle (get unlock-height account))) rc)
            (err ERR-STACKER-DOES-NOT-UNLOCK))

        (ok true)
    ))

;; Method to register the signer.
;; The signer's tenure will begin in the next reward cycle.
;; No validity checking is done.
(define-private (inner-signer-register (signer principal)
                                       (amount-ustx uint)
                                       (cur-burn-ht uint)
                                       (public-key (buff 33))
                                       (payout-addr { version: (buff 1), hashbytes: (buff 32) }))
    (let (
        (rc (+ u1 (get-reward-cycle cur-burn-ht)))
        (dh (default-to 0x0000000000000000000000000000000000000000000000000000000000000000
            (map-get? disbursement-hash rc)))
    )
        ;; register this stacker if it isn't already registered in this reward cycle (fails otherwise)
        (asserts! (map-insert signer-state
                    { signer: signer, reward-cycle: rc }
                    {
                        amount-ustx: amount-ustx,
                        public-key: public-key,
                        btc-earned: none,
                        payout-addr: payout-addr,
                        pox-addr-vote: none
                    })
            (err ERR-ALREADY-REGISTERED))

        ;; mark this key as used
        (unwrap-panic
            (if (map-insert signer-public-keys-used public-key true)
                (ok true)
                (err ERR-SIGNING-KEY-ALREADY-USED)))

        ;; mark this payout address as used
        (unwrap-panic
            (if (map-insert payout-addrs-in-reward-cycle payout-addr rc)
                (ok true)
                (err ERR-PAYOUT-ADDRESS-ALREADY-USED)))
 
        ;; register this payout address for this reward cycle
        (unwrap-panic
            (if (map-insert payout-addr-state
                    { reward-cycle: rc, payout-addr: payout-addr }
                    {
                        amount-ustx: amount-ustx,
                        signer: signer
                    })
                (ok true)
                (err ERR-PAYOUT-ADDRESS-ALREADY-USED)))

        ;; update disbursement info
        (bump-disbursement-requirements rc amount-ustx)
       
        ;; update disbursement hash
        (map-set disbursement-hash rc (sha512/256 (concat dh public-key)))
        (ok true)
    ))

;; Register as a signer
(define-public (signer-register (signer principal)
                                (amount-ustx uint)
                                (cur-burn-ht uint)
                                (public-key (buff 33))
                                (payout-addr { version: (buff 1), hashbytes: (buff 32) }))
    (begin
        (try! (can-signer-register signer amount-ustx public-key cur-burn-ht payout-addr))
        (inner-signer-register signer amount-ustx cur-burn-ht public-key payout-addr)
    ))


;; Can a Stacker vote for an address?
(define-read-only (can-signer-vote (signer principal)
                                   (amount-ustx uint)
                                   (cur-burn-ht uint))
    (let (
        (rc (+ u1 (get-reward-cycle cur-burn-ht)))
        (signer-rec (map-get? signer-state { signer: signer, reward-cycle: rc }))
    )
        ;; we're in the voting window
        (asserts! (in-voting-window cur-burn-ht)
            (err ERR-NOT-VOTING-WINDOW))

        ;; all disbursements have been paid up to this cycle
        (asserts! (all-rewards-disbursed cur-burn-ht)
            (err ERR-MISSING-DISBURSEMENTS))

        ;; this signer has registered
        (asserts! (is-some signer-rec)
            (err ERR-SIGNER-NOT-REGISTERED))

        ;; this signer has not yet voted
        (asserts! (is-none (get pox-addr-vote (unwrap-panic signer-rec)))
            (err ERR-SIGNER-ALREADY-VOTED))

        ;; this signer has enough uSTX
        (asserts! (>= amount-ustx (var-get ustx-minimum))
            (err ERR-NOT-ENOUGH-STX))

        (ok (unwrap-panic signer-rec))
    ))

;; Cast a signer vote.
;; Does not lock up STX.
(define-private (inner-signer-vote (signer principal)
                                   (amount-ustx uint)
                                   (cur-burn-ht uint)
                                   (pox-addr { version: (buff 1), hashbytes: (buff 32) })
                                   (signer-rec {
                                       amount-ustx: uint,
                                       public-key: (buff 33),
                                       btc-earned: (optional uint),
                                       payout-addr: { version: (buff 1), hashbytes: (buff 32) },
                                       pox-addr-vote: (optional { version: (buff 1), hashbytes: (buff 32) })
                                   }))
    (let (
        (rc (+ u1 (get-reward-cycle cur-burn-ht)))
    )
        ;; cast the vote
        (map-set signer-state { signer: signer, reward-cycle: rc }
            (merge signer-rec {
                pox-addr-vote: (some pox-addr),
             }))
        (ok true)
    ))

;; Update the winning PoX addr
(define-private (update-winning-pox-addr (rc uint) (pox-addr { version: (buff 1), hashbytes: (buff 32) }))
    (let (
        (addr-status (unwrap-panic (map-get? voted-pox-addrs { reward-cycle: rc, pox-addr: pox-addr })))
        (winning-addr (default-to DEFAULT-WINNING-POX-ADDR (map-get? winning-pox-addrs rc)))
    )
        (match (map-get? voted-pox-addrs { pox-addr: winning-addr, reward-cycle: rc })
            best-so-far
                (if (> (get total-ustx addr-status) (get total-ustx best-so-far))
                    ;; new winner
                    (map-set winning-pox-addrs rc pox-addr)
                    ;; no change
                    true)
            ;; no current winner
            (map-set winning-pox-addrs rc pox-addr))
    ))

;; Carry out a signer vote.
;; When this call completes, the contract will record the vote for this PoX address from the caller, and will stack `amount-ustx`.
;; Only someone who has already pre-registered as a signer can call this.
(define-public (signer-vote (amount-ustx uint)
                            (pox-addr { version: (buff 1), hashbytes: (buff 32) }))
    (let (
        (rc (+ u1 (get-reward-cycle burn-block-height)))
        (signer-rec (try! (can-signer-vote tx-sender amount-ustx burn-block-height)))
    )
        (unwrap-panic (inner-signer-vote tx-sender amount-ustx burn-block-height pox-addr signer-rec))

        ;; stack and store vote information
        (let (
            (new-vote-rec (stack-returning-signer-ustx tx-sender amount-ustx pox-addr burn-block-height))
        )
            (map-set voted-pox-addrs { reward-cycle: rc, pox-addr: pox-addr } new-vote-rec)
        )

        ;; update winner
        (update-winning-pox-addr rc pox-addr)
        (ok true)
    ))

;; Get the winning PoX address for the upcoming reward cycle.
;; Only works if voting period for the upcoming reward cycle's sBTC wallet address is closed.
(define-read-only (inner-get-sbtc-wallet-addr (burn-ht uint))
    (let (
        (rc (+ u1 (get-reward-cycle burn-ht)))
    )
        (if (or (in-transfer-window burn-ht) (in-penalty-window))
            ;; after registration and voting period
            (map-get? winning-pox-addrs rc)
            none
        )
    ))

;; Get the sBTC wallet
(define-read-only (get-sbtc-wallet-addr)
    (inner-get-sbtc-wallet-addr burn-block-height))

;; Determine how many signing slots a signer has for a given reward cycle
;; This determines how many shares of the signature this stacker must contribute.
(define-read-only (get-signing-slots (signer principal) (reward-cycle uint))
    (match (map-get? signer-state { signer: signer, reward-cycle: reward-cycle })
        signer-rec
            (/ (get amount-ustx signer-rec) (var-get ustx-minimum))
        u0))
