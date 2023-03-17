(begin
    ;; first-block-height, reward-cycle-length, token-reward-maturity, max-reward-cycles
    (unwrap-panic (configure u1 u5 u3 u32))
)

(define-public (list-tests)
    (begin
       (print "test: unit-tests")
       (print "test: block-4")
       (print "test: block-5")
       (print "test: block-6")
       (print "test: block-7")
       (print "test: block-8")
       (print "test: block-9")
       (print "test: block-10")
       (print "test: block-11")
       (ok true)
    )
)

(define-public (unit-tests)

)

(define-public (block-4)
    (let (
        (rc (unwrap-panic (get-reward-cycle block-height)))
    )
    (begin
        (print "block-4: mine and stack tokens")
        (asserts! (is-eq u4 block-height) (err "Invalid block height"))

        (ok u0)
    ))
)

(define-public (block-5)
    (let (
        (rc (unwrap-panic (get-reward-cycle block-height)))
    )
    (begin
        (print "block-5")
        (asserts! (is-eq u5 block-height) (err "Invalid block height"))

        (ok u0)
    ))
)

(define-public (block-6)
    (let (
        (rc (unwrap-panic (get-reward-cycle block-height)))
    )
    (begin
        (print "block-6")
        (asserts! (is-eq u6 block-height) (err "Invalid block height"))

        (ok u0)
    ))
)

(define-public (block-7)
    (let (
        (rc (unwrap-panic (get-reward-cycle block-height)))
    )
    (begin
        (print "block-7")
        (asserts! (is-eq u7 block-height) (err "Invalid block height"))

        (ok u0)
    ))
)

(define-public (block-8)
    (let (
        (rc (unwrap-panic (get-reward-cycle block-height)))
    )
    (begin
        (print "block-8")
        (asserts! (is-eq u8 block-height) (err "Invalid block height"))

        (ok u0)
    ))
)

(define-public (block-9)
    (let (
        (rc (unwrap-panic (get-reward-cycle block-height)))
    )
    (begin
        (print "block-9")
        (asserts! (is-eq u9 block-height) (err "Invalid block height"))

        (ok u0)
    ))
)

(define-public (block-10)
    (let (
        (rc (unwrap-panic (get-reward-cycle block-height)))
    )
    (begin
        (print "block-10")
        (asserts! (is-eq u10 block-height) (err "Invalid block height"))

        (ok u0)
    ))
)

(define-public (block-11)
    (let (
        (rc (unwrap-panic (get-reward-cycle block-height)))
        (stx-before (stx-get-balance tx-sender))
    )
    (begin
        (print "block-11")
        (asserts! (is-eq u11 block-height) (err "Invalid block height"))

        (ok u0)
    ))
)
