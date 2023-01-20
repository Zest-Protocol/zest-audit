(impl-trait .traits.executor-trait)
(use-trait safe-trait .traits.safe-trait)
(use-trait nft-trait .traits.sip-009-trait)
(use-trait ft-trait .traits.sip-010-trait)

(define-public (execute (safe <safe-trait>) (param-ft <ft-trait>) (param-nft <nft-trait>) (param-p (optional principal)) (param-u (optional uint)) (param-b (optional (buff 20))))
  (begin
    (try!  (contract-call?
      .supplier-interface send-funds
      {
        header: 0x0200000001f01c1021c9d15a6ddda9e7f016586c5e1e57e8b456a90a5f741238a3ea5f01b1010000006a47304402206f2e00a06d84a629d1583d3e37d046dc768346e9cfb9f29a54fca8e25401661a02,
        height: u9
      }
      (list)
      0x0200000001f01c1021c9d15a6ddda9e7f016586c5e1e57e8b456a90a5f741238a3ea5f01b1010000006a47304402206f2e00a06d84a629d1583d3e37d046dc768346e9cfb9f29a54fca8e25401661a022055bfb842f1baaad40da4ff1e53431c30e383007ac53b9a93c938423c3d217b950121031aa68bfad0576216e20a30892af32e49948fbd2892c339c373bc28e49e04f9bffdffffff0200e1f5050000000017a91473b1a47e530561dd4d300e96e7ca53526616d9b387c0da0000000000001976a914c5b8cc55ab829cc07b08936234b14a039a38f4e288ac89692200
      {
          tx-index: u0,
          hashes: (list),
          tree-depth: u0
      }
      u0
      0x03d9235aa65e142767d489884572c24751f10a7c792fc3ade7eb299df4ab9e3104
      0x0201730264a6e295320cac777559ecf5fc5fe3679457db9eb1539ff7ef90d212c5
      0xf401
      0xbdafb05001fa4243666508c71f5cbbd9cc63577e694a31fe14f1302832802000
      0x00000000
      u0
      u100000
      ))
    (ok true)
  )
)