(impl-trait .traits.executor-trait)
(use-trait safe-trait .traits.safe-trait)
(use-trait nft-trait .traits.sip-009-trait)
(use-trait ft-trait .traits.sip-010-trait)

(define-public (execute (safe <safe-trait>) (param-ft <ft-trait>) (param-nft <nft-trait>) (param-p (optional principal)) (param-u (optional uint)) (param-b (optional (buff 20))))
  (begin
    (try!  (contract-call?
      .pool-v1-0 fund-loan
      (unwrap! param-u (err u9999))
      .lp-token
      u0
      .liquidity-vault-v1-0
      .funding-vault
      .Wrapped-Bitcoin))
    (ok true)
  )
)