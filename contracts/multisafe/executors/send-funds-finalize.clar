(impl-trait .traits.executor-trait)
(use-trait safe-trait .traits.safe-trait)
(use-trait nft-trait .traits.sip-009-trait)
(use-trait ft-trait .traits.sip-010-trait)

(define-public (execute (safe <safe-trait>) (param-ft <ft-trait>) (param-nft <nft-trait>) (param-p (optional principal)) (param-u (optional uint)) (param-b (optional (buff 20))))
  (begin
    (try!  (contract-call?
      .supplier-interface send-funds-finalize
      0x9331abe8b7f4971727df664003e7c043fce161736eb9825b503fab1aabdb14fa
      0x00
      u1
      .lp-token
      u0
      .zest-reward-dist
      .liquidity-vault-v1-0
      .Wrapped-Bitcoin
      .rewards-calc
      ))
    (ok true)
  )
)
